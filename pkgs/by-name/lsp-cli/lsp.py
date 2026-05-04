"""lsp — LSP client CLI for agents.

Manages long-lived LSP server sessions over Unix domain sockets, exposing
code-intelligence features (hover, definition, references, diagnostics,
completion, symbols, formatting) as shell commands.

Architecture
~~~~~~~~~~~~
``lsp session start --lsp gopls`` forks a daemon that owns the LSP server
subprocess (JSON-RPC over stdio).  Subsequent CLI calls connect to the
daemon's Unix socket, send a single-line JSON request, and read back a
single-line JSON response.

Session key: ``{project_root_hash}_{lsp_name}`` — one server per project.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import signal
import socket
import subprocess
import sys
import threading
import time
from contextlib import suppress
from pathlib import Path, PurePosixPath
from typing import Any

# ---------------------------------------------------------------------------
# LSP server registry
# ---------------------------------------------------------------------------

LSP_SERVERS: dict[str, dict[str, Any]] = {
    "gopls": {
        "command": "gopls",
        "args": [],
        "projectMarkers": ["go.mod", "go.sum"],
    },
    "typescript-language-server": {
        "command": "typescript-language-server",
        "args": ["--stdio"],
        "projectMarkers": ["tsconfig.json", "jsconfig.json", "package.json"],
    },
    "pyright": {
        "command": "pyright-langserver",
        "args": ["--stdio"],
        "projectMarkers": ["pyproject.toml", "setup.py", "setup.cfg", "requirements.txt"],
    },
    "nixd": {
        "command": "nixd",
        "args": [],
        "projectMarkers": ["flake.nix", "default.nix", "shell.nix"],
    },
    "clangd": {
        "command": "clangd",
        "args": [],
        "projectMarkers": ["compile_commands.json", "CMakeLists.txt", "Makefile"],
    },
    "rust-analyzer": {
        "command": "rust-analyzer",
        "args": [],
        "projectMarkers": ["Cargo.toml"],
    },
    "bash-language-server": {
        "command": "bash-language-server",
        "args": ["start"],
        "projectMarkers": [".git", ".jj"],
    },
    "yaml-language-server": {
        "command": "yaml-language-server",
        "args": ["--stdio"],
        "projectMarkers": [".git", ".jj"],
    },
    "intelephense": {
        "command": "intelephense",
        "args": ["--stdio"],
        "projectMarkers": ["composer.json", "index.php", ".git", ".jj"],
    },
}

# ---------------------------------------------------------------------------
# Language ID mapping  (file extension → LSP languageId)
# ---------------------------------------------------------------------------

LANG_ID: dict[str, str] = {
    ".go": "go",
    ".ts": "typescript",
    ".tsx": "typescriptreact",
    ".js": "javascript",
    ".jsx": "javascriptreact",
    ".py": "python",
    ".nix": "nix",
    ".c": "c",
    ".h": "c",
    ".cpp": "cpp",
    ".hpp": "cpp",
    ".cc": "cpp",
    ".cxx": "cpp",
    ".rs": "rust",
    ".sh": "shellscript",
    ".bash": "shellscript",
    ".yaml": "yaml",
    ".yml": "yaml",
    ".php": "php",
}

# ---------------------------------------------------------------------------
# URI helpers
# ---------------------------------------------------------------------------


def path_to_uri(path: str) -> str:
    """Convert an absolute file path to a file:// URI."""
    p = PurePosixPath(Path(path).resolve())
    return f"file:///{str(p).lstrip('/')}"


def uri_to_path(uri: str) -> str:
    """Convert a file:// URI to an absolute file path."""
    if uri.startswith("file:///"):
        return uri[len("file://"):]
    if uri.startswith("file://"):
        # Non-local host — shouldn't happen for us
        return uri[len("file://"):]
    return uri


# ---------------------------------------------------------------------------
# JSON-RPC transport (Content-Length framing over stdio)
# ---------------------------------------------------------------------------


class JsonRpcClient:
    """Low-level JSON-RPC 2.0 client over stdio pipes.

    Handles Content-Length framing, background reading, and request/response
    correlation.  Server-initiated notifications are buffered in a queue.
    """

    def __init__(
        self,
        command: str,
        args: list[str],
        root_path: str,
        log_file: Any = None,
    ) -> None:
        self._proc = subprocess.Popen(
            [command] + args,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=log_file or subprocess.DEVNULL,
            cwd=root_path,
        )
        self._id_counter = 0
        self._lock = threading.Lock()
        # id → {"event": threading.Event, "result": dict|None, "error": dict|None}
        self._pending: dict[int, dict[str, Any]] = {}
        self._notifications: list[dict[str, Any]] = []
        self._notif_lock = threading.Lock()
        self._reader_thread = threading.Thread(target=self._read_loop, daemon=True)
        self._reader_thread.start()

    # -- framing -----------------------------------------------------------

    def _write_frame(self, data: dict[str, Any]) -> None:
        body = json.dumps(data).encode("utf-8")
        header = f"Content-Length: {len(body)}\r\n\r\n".encode("ascii")
        if self._proc.stdin is None:
            raise RuntimeError("stdin pipe closed")
        self._proc.stdin.write(header)
        self._proc.stdin.write(body)
        self._proc.stdin.flush()

    def _read_frame(self) -> dict[str, Any]:
        """Read one JSON-RPC frame from the server's stdout."""
        if self._proc.stdout is None:
            raise RuntimeError("stdout pipe closed")
        # Read headers until empty line
        content_length: int | None = None
        while True:
            line = self._proc.stdout.readline()
            if not line:
                raise RuntimeError("LSP server closed stdout")
            line_str = line.decode("ascii").strip()
            if line_str == "":
                break
            if line_str.lower().startswith("content-length:"):
                content_length = int(line_str.split(":", 1)[1].strip())
        if content_length is None:
            raise RuntimeError("missing Content-Length header")
        body = self._proc.stdout.read(content_length)
        if len(body) < content_length:
            raise RuntimeError("truncated JSON-RPC body")
        return json.loads(body)

    # -- background reader -------------------------------------------------

    def _read_loop(self) -> None:
        """Background thread: read frames, dispatch responses, buffer notifications."""
        try:
            while True:
                try:
                    frame = self._read_frame()
                except RuntimeError:
                    break
                msg_id = frame.get("id")
                if msg_id is not None and "method" not in frame:
                    # This is a response — correlate by id
                    with self._lock:
                        entry = self._pending.get(msg_id)
                    if entry is not None:
                        if "result" in frame:
                            entry["result"] = frame["result"]
                        if "error" in frame:
                            entry["error"] = frame["error"]
                        entry["event"].set()
                elif "method" in frame:
                    # Notification or server-initiated request
                    with self._notif_lock:
                        self._notifications.append(frame)
                    if msg_id is not None:
                        # Server-initiated request — we don't handle these,
                        # but respond with empty result to avoid blocking
                        self._write_frame(
                            {"jsonrpc": "2.0", "id": msg_id, "result": None}
                        )
        except Exception:
            pass

    # -- public API --------------------------------------------------------

    def next_id(self) -> int:
        self._id_counter += 1
        return self._id_counter

    def send_request(
        self,
        method: str,
        params: dict[str, Any] | None = None,
        timeout: float = 30.0,
    ) -> Any:
        """Send a JSON-RPC request and block until the response arrives."""
        msg_id = self.next_id()
        req: dict[str, Any] = {"jsonrpc": "2.0", "id": msg_id, "method": method}
        if params is not None:
            req["params"] = params

        event = threading.Event()
        entry: dict[str, Any] = {"event": event, "result": None, "error": None}
        with self._lock:
            self._pending[msg_id] = entry

        self._write_frame(req)

        if not event.wait(timeout=timeout):
            with self._lock:
                self._pending.pop(msg_id, None)
            raise TimeoutError(f"LSP request {method!r} timed out after {timeout}s")

        with self._lock:
            self._pending.pop(msg_id, None)

        if entry["error"] is not None:
            err = entry["error"]
            raise RuntimeError(
                f"LSP error for {method!r}: "
                f"code={err.get('code')}, message={err.get('message')}"
            )
        return entry["result"]

    def send_notification(
        self, method: str, params: dict[str, Any] | None = None
    ) -> None:
        """Send a JSON-RPC notification (no response expected)."""
        msg: dict[str, Any] = {"jsonrpc": "2.0", "method": method}
        if params is not None:
            msg["params"] = params
        self._write_frame(msg)

    def drain_notifications(self) -> list[dict[str, Any]]:
        """Return and clear all buffered server notifications."""
        with self._notif_lock:
            notifs = list(self._notifications)
            self._notifications.clear()
        return notifs

    @property
    def returncode(self) -> int | None:
        return self._proc.returncode

    def terminate(self) -> None:
        with suppress(Exception):
            self._proc.terminate()

    def kill(self) -> None:
        with suppress(Exception):
            self._proc.kill()

    def wait(self, timeout: float = 5.0) -> int | None:
        try:
            return self._proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            return None


# ---------------------------------------------------------------------------
# LSP session (high-level)
# ---------------------------------------------------------------------------


class LspSession:
    """High-level LSP session on top of JsonRpcClient.

    Manages the initialize/initialized handshake, open file tracking,
    diagnostic collection, and clean shutdown.
    """

    def __init__(self, client: JsonRpcClient, root_path: str) -> None:
        self._client = client
        self._root_path = root_path
        self._root_uri = path_to_uri(root_path)
        self._open_files: dict[str, int] = {}  # uri → version
        self._diagnostics: dict[str, list[dict[str, Any]]] = {}  # uri → diagnostics
        self._server_caps: dict[str, Any] = {}
        self._initialized = False
        self._lock = threading.Lock()

    # -- initialization ----------------------------------------------------

    def initialize(self) -> dict[str, Any]:
        """Perform LSP initialize/initialized handshake."""
        result = self._client.send_request(
            "initialize",
            {
                "processId": os.getpid(),
                "rootUri": self._root_uri,
                "workspaceFolders": [
                    {
                        "uri": self._root_uri,
                        "name": Path(self._root_path).name,
                    }
                ],
                "capabilities": {
                    "textDocument": {
                        "hover": {"contentFormat": ["markdown", "plaintext"]},
                        "completion": {
                            "completionItem": {
                                "snippetSupport": False,
                            },
                        },
                        "publishDiagnostics": {
                            "relatedInformation": True,
                        },
                    },
                    "workspace": {
                        "symbol": {"symbolKind": {"valueSet": list(range(1, 27))}},
                    },
                },
            },
        )
        self._server_caps = result.get("capabilities", {})

        # Send initialized notification
        self._client.send_notification("initialized", {})
        self._initialized = True

        # Drain any early notifications (e.g. publishDiagnostics)
        self._collect_notifications()
        return result

    # -- file management ---------------------------------------------------

    def open_file(self, file_path: str) -> None:
        """Send textDocument/didOpen for a file."""
        abs_path = str(Path(file_path).resolve())
        uri = path_to_uri(abs_path)
        with self._lock:
            if uri in self._open_files:
                return  # already open
        try:
            text = Path(abs_path).read_text()
        except OSError as exc:
            raise RuntimeError(f"cannot read {abs_path}: {exc}") from exc
        ext = PurePosixPath(abs_path).suffix
        lang = LANG_ID.get(ext, "")
        version = 1
        with self._lock:
            self._open_files[uri] = version
        self._client.send_notification(
            "textDocument/didOpen",
            {
                "textDocument": {
                    "uri": uri,
                    "languageId": lang,
                    "version": version,
                    "text": text,
                },
            },
        )
        self._collect_notifications()

    def close_file(self, file_path: str) -> None:
        """Send textDocument/didClose for a file."""
        abs_path = str(Path(file_path).resolve())
        uri = path_to_uri(abs_path)
        with self._lock:
            if uri not in self._open_files:
                return
            del self._open_files[uri]
        self._client.send_notification(
            "textDocument/didClose",
            {"textDocument": {"uri": uri}},
        )

    def ensure_open(self, file_path: str) -> None:
        """Idempotent: open file if not already open."""
        abs_path = str(Path(file_path).resolve())
        uri = path_to_uri(abs_path)
        with self._lock:
            if uri in self._open_files:
                return
        self.open_file(abs_path)

    # -- notification collector --------------------------------------------

    def _collect_notifications(self) -> None:
        """Drain buffered notifications and update state."""
        for notif in self._client.drain_notifications():
            method = notif.get("method")
            params = notif.get("params", {})
            if method == "textDocument/publishDiagnostics":
                uri = params.get("uri", "")
                diags = params.get("diagnostics", [])
                self._diagnostics[uri] = diags
            # Ignore other notifications for now

    # -- LSP features ------------------------------------------------------

    def hover(self, file_path: str, line: int, character: int) -> Any:
        self.ensure_open(file_path)
        self._collect_notifications()
        result = self._client.send_request(
            "textDocument/hover",
            {
                "textDocument": {"uri": path_to_uri(str(Path(file_path).resolve()))},
                "position": {"line": line, "character": character},
            },
        )
        self._collect_notifications()
        return result

    def definition(self, file_path: str, line: int, character: int) -> Any:
        self.ensure_open(file_path)
        self._collect_notifications()
        result = self._client.send_request(
            "textDocument/definition",
            {
                "textDocument": {"uri": path_to_uri(str(Path(file_path).resolve()))},
                "position": {"line": line, "character": character},
            },
        )
        self._collect_notifications()
        return _normalize_locations(result)

    def references(
        self,
        file_path: str,
        line: int,
        character: int,
        include_declaration: bool = True,
    ) -> Any:
        self.ensure_open(file_path)
        self._collect_notifications()
        result = self._client.send_request(
            "textDocument/references",
            {
                "textDocument": {"uri": path_to_uri(str(Path(file_path).resolve()))},
                "position": {"line": line, "character": character},
                "context": {"includeDeclaration": include_declaration},
            },
        )
        self._collect_notifications()
        return result

    def diagnostics(self, file_path: str | None = None) -> Any:
        """Return accumulated diagnostics (push model via publishDiagnostics)."""
        self._collect_notifications()
        if file_path is not None:
            uri = path_to_uri(str(Path(file_path).resolve()))
            return self._diagnostics.get(uri, [])
        return self._diagnostics

    def completion(self, file_path: str, line: int, character: int) -> Any:
        self.ensure_open(file_path)
        self._collect_notifications()
        result = self._client.send_request(
            "textDocument/completion",
            {
                "textDocument": {"uri": path_to_uri(str(Path(file_path).resolve()))},
                "position": {"line": line, "character": character},
            },
        )
        self._collect_notifications()
        return result

    def document_symbols(self, file_path: str) -> Any:
        self.ensure_open(file_path)
        self._collect_notifications()
        result = self._client.send_request(
            "textDocument/documentSymbol",
            {"textDocument": {"uri": path_to_uri(str(Path(file_path).resolve()))}},
        )
        self._collect_notifications()
        return result

    def workspace_symbols(self, query: str = "") -> Any:
        self._collect_notifications()
        result = self._client.send_request(
            "workspace/symbol",
            {"query": query},
        )
        self._collect_notifications()
        return result

    def formatting(self, file_path: str) -> Any:
        self.ensure_open(file_path)
        self._collect_notifications()
        result = self._client.send_request(
            "textDocument/formatting",
            {
                "textDocument": {"uri": path_to_uri(str(Path(file_path).resolve()))},
                "options": {"tabSize": 4, "insertSpaces": True},
            },
        )
        self._collect_notifications()
        return result

    # -- shutdown ----------------------------------------------------------

    def shutdown(self) -> None:
        """Shut down the LSP server gracefully."""
        with suppress(Exception):
            self._client.send_request("shutdown", timeout=5.0)
        with suppress(Exception):
            self._client.send_notification("exit")
        with suppress(Exception):
            self._client.wait(timeout=5.0)
        self._client.terminate()


# ---------------------------------------------------------------------------
# Location normalization
# ---------------------------------------------------------------------------


def _normalize_locations(result: Any) -> Any:
    """Normalize definition results to a consistent list of locations."""
    if result is None:
        return []
    # Location
    if isinstance(result, dict) and "uri" in result:
        return [result]
    # Location[]
    if isinstance(result, list):
        out: list[dict[str, Any]] = []
        for item in result:
            if isinstance(item, dict) and "uri" in item:
                out.append(item)
            elif isinstance(item, dict) and "targetUri" in item:
                # LocationLink → Location
                out.append(
                    {
                        "uri": item["targetUri"],
                        "range": item.get("targetRange", item.get("targetSelectionRange", {})),
                    }
                )
        return out
    return result


# ---------------------------------------------------------------------------
# Project root detection
# ---------------------------------------------------------------------------

_VCS_MARKERS = [".git", ".jj", ".hg"]


def find_project_root(start_dir: str, lsp_name: str) -> str | None:
    """Walk upward from *start_dir* looking for project markers, then VCS markers."""
    cfg = LSP_SERVERS.get(lsp_name)
    markers = cfg["projectMarkers"] if cfg else []
    d = Path(start_dir).resolve()
    # LSP-specific markers first
    while True:
        for m in markers:
            if (d / m).exists():
                return str(d)
        parent = d.parent
        if parent == d:
            break
        d = parent
    # VCS fallback
    d = Path(start_dir).resolve()
    while True:
        for m in _VCS_MARKERS:
            p = d / m
            if p.is_dir() or p.exists():
                return str(d)
        parent = d.parent
        if parent == d:
            break
        d = parent
    return None


# ---------------------------------------------------------------------------
# Session management helpers
# ---------------------------------------------------------------------------


def _session_dir() -> Path:
    base = os.environ.get("XDG_RUNTIME_DIR")
    if not base:
        base = f"/tmp/lsp-cli-{os.getuid()}"
    d = Path(base) / "lsp-cli"
    d.mkdir(parents=True, exist_ok=True)
    return d


def _session_key(lsp_name: str, project_root: str) -> str:
    h = hashlib.sha256(project_root.encode()).hexdigest()[:12]
    return f"{h}_{lsp_name}"


def _session_paths(key: str) -> dict[str, Path]:
    d = _session_dir()
    return {
        "sock": d / f"{key}.sock",
        "pid": d / f"{key}.pid",
        "meta": d / f"{key}.json",
        "log": d / f"{key}.log",
    }


def _read_pid(paths: dict[str, Path]) -> int | None:
    try:
        return int(paths["pid"].read_text().strip())
    except (OSError, ValueError):
        return None


def _is_pid_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
        return True
    except ProcessLookupError:
        return False
    except PermissionError:
        return True


def _cleanup_stale(paths: dict[str, Path]) -> None:
    for p in [paths["sock"], paths["pid"], paths["meta"]]:
        with suppress(OSError):
            p.unlink()


def _resolve_lsp_and_root(args: argparse.Namespace) -> tuple[str, str]:
    """Resolve the LSP name and project root from CLI args."""
    lsp_name = args.lsp
    root = getattr(args, "root", None)
    if root:
        return lsp_name, str(Path(root).resolve())
    detected = find_project_root(os.getcwd(), lsp_name)
    if detected is None:
        print(
            f"lsp: cannot detect project root for {lsp_name!r}; "
            f"use --root DIR",
            file=sys.stderr,
        )
        sys.exit(1)
    return lsp_name, detected


# ---------------------------------------------------------------------------
# Socket IPC helpers
# ---------------------------------------------------------------------------


def _session_call(key: str, req: dict[str, Any], timeout_s: float = 120.0) -> dict[str, Any]:
    """Send a request to the daemon over its Unix socket."""
    paths = _session_paths(key)
    sock_path = paths["sock"]
    if not sock_path.exists():
        print(f"lsp: session {key!r} not running (socket missing)", file=sys.stderr)
        sys.exit(1)

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.settimeout(timeout_s)
    try:
        sock.connect(str(sock_path))
        sock.sendall((json.dumps(req) + "\n").encode("utf-8"))
        buf = b""
        while True:
            chunk = sock.recv(65536)
            if not chunk:
                break
            buf += chunk
            if b"\n" in buf:
                break
        if not buf.strip():
            raise RuntimeError("empty response from daemon")
        return json.loads(buf.decode("utf-8").strip())
    except socket.timeout:
        raise TimeoutError(f"daemon call timed out after {timeout_s}s")
    finally:
        sock.close()


# ---------------------------------------------------------------------------
# Daemon
# ---------------------------------------------------------------------------


def _daemon_main(args: argparse.Namespace) -> int:
    """Main loop for the background daemon."""
    lsp_name = args.lsp
    root_path = args.root
    key = _session_key(lsp_name, root_path)
    paths = _session_paths(key)

    cfg = LSP_SERVERS[lsp_name]
    command = cfg["command"]
    cmd_args = cfg["args"]

    # Write PID
    paths["pid"].write_text(str(os.getpid()))
    paths["meta"].write_text(
        json.dumps(
            {
                "lsp": lsp_name,
                "projectRoot": root_path,
                "pid": os.getpid(),
                "started": time.time(),
            }
        )
    )

    # Clean stale socket
    if paths["sock"].exists():
        paths["sock"].unlink()

    # Start LSP server
    log_file = paths["log"].open("wb")
    try:
        client = JsonRpcClient(command, cmd_args, root_path, log_file=log_file)
    except Exception as exc:
        print(f"lsp: failed to start {command}: {exc}", file=sys.stderr)
        _cleanup_stale(paths)
        return 1

    # Initialize
    session = LspSession(client, root_path)
    try:
        session.initialize()
    except Exception as exc:
        print(f"lsp: initialize failed for {lsp_name}: {exc}", file=sys.stderr)
        client.terminate()
        _cleanup_stale(paths)
        return 1

    stop_event = threading.Event()
    request_lock = threading.Lock()

    # Signal handling
    loop = threading.current_thread()
    for sig in (signal.SIGTERM, signal.SIGINT):
        signal.signal(sig, lambda *_: stop_event.set())

    # Unix socket server
    import socketserver

    class Handler(socketserver.BaseRequestHandler):
        def handle(self) -> None:  # type: ignore[override]
            try:
                raw = self.request.recv(65536)
                if not raw:
                    return
                req = json.loads(raw.decode("utf-8").strip())
            except Exception:
                return

            with request_lock:
                try:
                    result = _daemon_dispatch(session, req)
                    resp: dict[str, Any] = {"ok": True, "result": result}
                except Exception as exc:
                    resp = {"ok": False, "error": f"{type(exc).__name__}: {exc}"}

            try:
                self.request.sendall((json.dumps(resp, default=str) + "\n").encode("utf-8"))
            except Exception:
                pass

    class UnixServer(socketserver.ThreadingMixIn, socketserver.UnixStreamServer):
        allow_reuse_address = True  # type: ignore[assignment]

    try:
        server = UnixServer(str(paths["sock"]), Handler)
    except Exception as exc:
        print(f"lsp: failed to create socket: {exc}", file=sys.stderr)
        session.shutdown()
        _cleanup_stale(paths)
        return 1

    with suppress(OSError):
        paths["sock"].chmod(0o600)

    # Serve in a thread so we can wait on stop_event
    server_thread = threading.Thread(target=server.serve_forever, daemon=True)
    server_thread.start()

    try:
        stop_event.wait()
    finally:
        server.shutdown()
        server.server_close()
        session.shutdown()
        _cleanup_stale(paths)
        log_file.close()

    return 0


def _daemon_dispatch(session: LspSession, req: dict[str, Any]) -> Any:
    """Dispatch a CLI request to the appropriate LspSession method."""
    method = req.get("method")
    if method == "hover":
        return session.hover(req["file"], req["line"], req["character"])
    if method == "definition":
        return session.definition(req["file"], req["line"], req["character"])
    if method == "references":
        return session.references(
            req["file"], req["line"], req["character"],
            req.get("includeDeclaration", True),
        )
    if method == "diagnostics":
        return session.diagnostics(req.get("file"))
    if method == "completion":
        return session.completion(req["file"], req["line"], req["character"])
    if method == "documentSymbol":
        return session.document_symbols(req["file"])
    if method == "workspaceSymbol":
        return session.workspace_symbols(req.get("query", ""))
    if method == "formatting":
        return session.formatting(req["file"])
    raise ValueError(f"unknown method: {method!r}")


# ---------------------------------------------------------------------------
# CLI subcommands
# ---------------------------------------------------------------------------


def cmd_session_start(args: argparse.Namespace) -> int:
    lsp_name, root = _resolve_lsp_and_root(args)
    key = _session_key(lsp_name, root)
    paths = _session_paths(key)

    pid = _read_pid(paths)
    if pid is not None and _is_pid_alive(pid):
        print(f"lsp: session {lsp_name!r} already running (pid {pid})", file=sys.stderr)
        return 1
    _cleanup_stale(paths)

    cmd = [
        sys.executable,
        sys.argv[0],
        "__daemon__",
        "--lsp", lsp_name,
        "--root", root,
    ]

    log = paths["log"].open("wb")
    proc = subprocess.Popen(
        cmd,
        stdin=subprocess.DEVNULL,
        stdout=log,
        stderr=log,
        start_new_session=True,
    )

    deadline = time.monotonic() + 30.0
    while time.monotonic() < deadline:
        if paths["sock"].exists() and paths["pid"].exists():
            print(key)
            return 0
        if proc.poll() is not None:
            print(
                f"lsp: daemon exited before becoming ready; see {paths['log']}",
                file=sys.stderr,
            )
            return 1
        time.sleep(0.1)

    print(f"lsp: timed out waiting for session {key!r}", file=sys.stderr)
    proc.terminate()
    return 1


def cmd_session_stop(args: argparse.Namespace) -> int:
    lsp_name, root = _resolve_lsp_and_root(args)
    key = _session_key(lsp_name, root)
    paths = _session_paths(key)

    pid = _read_pid(paths)
    if pid is None:
        _cleanup_stale(paths)
        if not args.quiet:
            print(f"lsp: no session for {lsp_name!r}", file=sys.stderr)
        return 0 if args.quiet else 1

    try:
        os.kill(pid, signal.SIGTERM)
    except ProcessLookupError:
        _cleanup_stale(paths)
        return 0

    deadline = time.monotonic() + 10.0
    while time.monotonic() < deadline:
        if not _is_pid_alive(pid):
            _cleanup_stale(paths)
            return 0
        time.sleep(0.1)

    with suppress(ProcessLookupError):
        os.kill(pid, signal.SIGKILL)
    _cleanup_stale(paths)
    return 0


def cmd_session_list(args: argparse.Namespace) -> int:  # noqa: ARG001
    d = _session_dir()
    sessions: list[dict[str, Any]] = []
    for meta_path in sorted(d.glob("*.json")):
        try:
            meta = json.loads(meta_path.read_text())
        except (OSError, json.JSONDecodeError):
            continue
        name = meta_path.stem
        paths = _session_paths(name)
        pid = _read_pid(paths)
        alive = pid is not None and _is_pid_alive(pid)
        if not alive:
            _cleanup_stale(paths)
            continue
        sessions.append(meta)
    for entry in sessions:
        _emit_json(entry)
    return 0


def _cmd_query(args: argparse.Namespace, method: str, *, file: str | None = None, line: int | None = None, character: int | None = None, include_declaration: bool = True, query: str = "") -> int:
    lsp_name, root = _resolve_lsp_and_root(args)
    key = _session_key(lsp_name, root)
    req: dict[str, Any] = {"method": method}
    if file is not None:
        req["file"] = str(Path(file).resolve())
    if line is not None:
        req["line"] = line
    if character is not None:
        req["character"] = character
    if method == "references":
        req["includeDeclaration"] = include_declaration
    if method == "workspaceSymbol":
        req["query"] = query
    resp = _session_call(key, req)
    if not resp.get("ok"):
        print(f"lsp: {resp.get('error', 'unknown error')}", file=sys.stderr)
        return 1
    _emit_json(resp.get("result"))
    return 0

def cmd_hover(args: argparse.Namespace) -> int:
    return _cmd_query(args, "hover", file=args.file, line=args.line, character=args.character)


def cmd_definition(args: argparse.Namespace) -> int:
    return _cmd_query(args, "definition", file=args.file, line=args.line, character=args.character)


def cmd_references(args: argparse.Namespace) -> int:
    return _cmd_query(args, "references", file=args.file, line=args.line, character=args.character)


def cmd_diagnostics(args: argparse.Namespace) -> int:
    file = args.file if args.file else None
    return _cmd_query(args, "diagnostics", file=file)


def cmd_completion(args: argparse.Namespace) -> int:
    return _cmd_query(args, "completion", file=args.file, line=args.line, character=args.character)


def cmd_symbols(args: argparse.Namespace) -> int:
    if args.file:
        return _cmd_query(args, "documentSymbol", file=args.file)
    return _cmd_query(args, "workspaceSymbol", query=getattr(args, "query", ""))


def cmd_format(args: argparse.Namespace) -> int:
    return _cmd_query(args, "formatting", file=args.file)


def cmd_daemon(args: argparse.Namespace) -> int:
    return _daemon_main(args)


# ---------------------------------------------------------------------------
# Output helpers
# ---------------------------------------------------------------------------


def _emit_json(data: Any) -> None:
    print(json.dumps(data, default=str))


# ---------------------------------------------------------------------------
# Argument parser
# ---------------------------------------------------------------------------


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="lsp",
        description=__doc__,
    )
    sub = parser.add_subparsers(dest="cmd", required=True)

    # -- session lifecycle -------------------------------------------------
    sess = sub.add_parser("session", help="manage LSP server sessions")
    sess_sub = sess.add_subparsers(dest="session_cmd", required=True)

    sp = sess_sub.add_parser("start", help="start an LSP server session")
    sp.add_argument("--lsp", required=True, choices=sorted(LSP_SERVERS),
                     help="LSP server name")
    sp.add_argument("--root", default=None, help="project root directory")
    sp.set_defaults(func=cmd_session_start)

    sp = sess_sub.add_parser("stop", help="stop an LSP server session")
    sp.add_argument("--lsp", required=True, choices=sorted(LSP_SERVERS),
                     help="LSP server name")
    sp.add_argument("--root", default=None, help="project root directory")
    sp.add_argument("--quiet", action="store_true",
                     help="exit 0 even if session was missing")
    sp.set_defaults(func=cmd_session_stop)

    sp = sess_sub.add_parser("list", help="list active sessions (JSONL)")
    sp.set_defaults(func=cmd_session_list)

    # -- query commands ----------------------------------------------------
    _add_lsp_arg = lambda p: p.add_argument(
        "--lsp", required=True, choices=sorted(LSP_SERVERS),
        help="LSP server name",
    )
    _add_root_arg = lambda p: p.add_argument(
        "--root", default=None, help="project root directory",
    )

    p = sub.add_parser("hover", help="hover information at position")
    _add_lsp_arg(p)
    _add_root_arg(p)
    p.add_argument("file", help="file path")
    p.add_argument("line", type=int, help="0-indexed line number")
    p.add_argument("character", type=int, help="0-indexed character offset")
    p.set_defaults(func=cmd_hover)

    p = sub.add_parser("definition", help="go to definition at position")
    _add_lsp_arg(p)
    _add_root_arg(p)
    p.add_argument("file", help="file path")
    p.add_argument("line", type=int, help="0-indexed line number")
    p.add_argument("character", type=int, help="0-indexed character offset")
    p.set_defaults(func=cmd_definition)

    p = sub.add_parser("references", help="find references at position")
    _add_lsp_arg(p)
    _add_root_arg(p)
    p.add_argument("file", help="file path")
    p.add_argument("line", type=int, help="0-indexed line number")
    p.add_argument("character", type=int, help="0-indexed character offset")
    p.add_argument("--no-declaration", action="store_true",
                    help="exclude the declaration site")
    p.set_defaults(func=cmd_references)

    p = sub.add_parser("diagnostics", help="get diagnostics for file or project")
    _add_lsp_arg(p)
    _add_root_arg(p)
    p.add_argument("file", nargs="?", default=None, help="file path (omit for all)")
    p.set_defaults(func=cmd_diagnostics)

    p = sub.add_parser("completion", help="completion items at position")
    _add_lsp_arg(p)
    _add_root_arg(p)
    p.add_argument("file", help="file path")
    p.add_argument("line", type=int, help="0-indexed line number")
    p.add_argument("character", type=int, help="0-indexed character offset")
    p.set_defaults(func=cmd_completion)

    p = sub.add_parser("symbols", help="document or workspace symbols")
    _add_lsp_arg(p)
    _add_root_arg(p)
    p.add_argument("file", nargs="?", default=None,
                    help="file path (omit for workspace symbols)")
    p.add_argument("--query", default="", help="workspace symbol query")
    p.set_defaults(func=cmd_symbols)

    p = sub.add_parser("format", help="format a file")
    _add_lsp_arg(p)
    _add_root_arg(p)
    p.add_argument("file", help="file path")
    p.set_defaults(func=cmd_format)

    # Hidden daemon entry point
    d = sub.add_parser("__daemon__")
    d.add_argument("--lsp", required=True)
    d.add_argument("--root", required=True)
    d.set_defaults(func=cmd_daemon)

    return parser


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        rc = args.func(args)
    except KeyboardInterrupt:
        return 130
    except TimeoutError as exc:
        print(f"lsp: timeout: {exc}", file=sys.stderr)
        return 124
    return rc if isinstance(rc, int) else 0


if __name__ == "__main__":
    sys.exit(main())

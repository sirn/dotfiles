"""web - headless Chromium CLI for agent skills."""

from __future__ import annotations

import argparse
import asyncio
import base64
import json
import os
import signal
import socket
import subprocess
import sys
import time
from contextlib import contextmanager, suppress
from pathlib import Path
from typing import Any

from playwright.sync_api import (
    Page,
    Response,
    sync_playwright,
)
from playwright.sync_api import (
    TimeoutError as PlaywrightTimeoutError,
)

####
## Common option parsing


def _viewport(value: str) -> dict[str, int]:
    try:
        w, h = value.lower().split("x", 1)
        return {"width": int(w), "height": int(h)}
    except ValueError as exc:
        raise argparse.ArgumentTypeError(
            f"viewport must be WxH, got {value!r}"
        ) from exc


def _header(value: str) -> tuple[str, str]:
    if ":" not in value:
        raise argparse.ArgumentTypeError(f"header must be 'Key: Value', got {value!r}")
    key, _, val = value.partition(":")
    return key.strip(), val.strip()


def _add_bimodal_common(parser: argparse.ArgumentParser) -> None:
    """Args shared by commands that run either one-shot or against a session."""
    parser.add_argument(
        "--session",
        default=None,
        metavar="NAME",
        help="run against a session instead of one-shot",
    )
    parser.add_argument("--timeout", type=int, default=30000, help="ms (default 30000)")
    parser.add_argument(
        "--wait-for",
        metavar="SELECTOR",
        default=None,
        help="(one-shot) wait for selector after load",
    )
    parser.add_argument(
        "--wait-until",
        choices=("domcontentloaded", "load", "networkidle", "commit"),
        default="domcontentloaded",
        help="(one-shot) navigation lifecycle event",
    )
    parser.add_argument(
        "--viewport",
        type=_viewport,
        default={"width": 1280, "height": 800},
        metavar="WxH",
        help="(one-shot) initial viewport",
    )
    parser.add_argument("--user-agent", default=None, help="(one-shot)")
    parser.add_argument(
        "--header",
        action="append",
        type=_header,
        default=[],
        metavar="K:V",
        help="(one-shot) extra request header; repeatable",
    )


def _add_session_arg(parser: argparse.ArgumentParser) -> None:
    """Args for session-only commands (always operate against a session)."""
    parser.add_argument("--session", required=True, help="session name")
    parser.add_argument("--timeout", type=int, default=30000, help="ms (default 30000)")


def _emit_json(obj: Any) -> None:
    json.dump(obj, sys.stdout, default=str)
    sys.stdout.write("\n")


def _write_bytes(output: str, data: bytes) -> None:
    if output == "-":
        sys.stdout.buffer.write(data)
    else:
        Path(output).write_bytes(data)


def _require_url(parser_name: str, value: str | None) -> str:
    if not value:
        sys.exit(f"web {parser_name}: URL required without --session")
    return value


####
## One-shot helpers


@contextmanager
def _open_page_oneshot(args: argparse.Namespace):
    with sync_playwright() as pw:
        browser = pw.chromium.launch(
            headless=True,
            channel="chromium",
            args=["--disable-dev-shm-usage"],
        )
        try:
            ctx_kwargs: dict[str, Any] = {"viewport": args.viewport}
            if args.user_agent:
                ctx_kwargs["user_agent"] = args.user_agent
            if args.header:
                ctx_kwargs["extra_http_headers"] = dict(args.header)
            context = browser.new_context(**ctx_kwargs)
            page = context.new_page()
            yield page
        finally:
            browser.close()


def _navigate_oneshot(page: Page, url: str, args: argparse.Namespace) -> None:
    page.goto(url, wait_until=args.wait_until, timeout=args.timeout)
    if args.wait_for:
        page.wait_for_selector(args.wait_for, timeout=args.timeout)


####
## Bimodal commands
## Take either a positional URL (one-shot) or `--session NAME` (attached).


def cmd_html(args: argparse.Namespace) -> int:
    if args.session:
        result = _session_call(
            args.session,
            {
                "op": "html",
                "selector": args.target,
                "timeout": args.timeout,
            },
        )
        sys.stdout.write(result)
        return 0
    url = _require_url("html", args.target)
    with _open_page_oneshot(args) as page:
        _navigate_oneshot(page, url, args)
        sys.stdout.write(page.content())
    return 0


def cmd_text(args: argparse.Namespace) -> int:
    if args.session:
        selector = args.target or "body"
        result = _session_call(
            args.session,
            {
                "op": "text",
                "selector": selector,
                "timeout": args.timeout,
            },
        )
        sys.stdout.write(result)
        if not result.endswith("\n"):
            sys.stdout.write("\n")
        return 0
    url = _require_url("text", args.target)
    with _open_page_oneshot(args) as page:
        _navigate_oneshot(page, url, args)
        sys.stdout.write(page.inner_text("body"))
    return 0


def cmd_screenshot(args: argparse.Namespace) -> int:
    if args.session:
        if args.url:
            sys.exit("web screenshot: positional URL not allowed with --session")
        if args.output == "-":
            data_b64 = _session_call(
                args.session,
                {
                    "op": "screenshot",
                    "selector": args.selector,
                    "full_page": args.full_page,
                    "inline": True,
                    "timeout": args.timeout,
                },
            )
            sys.stdout.buffer.write(base64.b64decode(data_b64))
        else:
            out = str(Path(args.output).resolve())
            _session_call(
                args.session,
                {
                    "op": "screenshot",
                    "selector": args.selector,
                    "full_page": args.full_page,
                    "path": out,
                    "timeout": args.timeout,
                },
            )
        return 0
    url = _require_url("screenshot", args.url)
    with _open_page_oneshot(args) as page:
        _navigate_oneshot(page, url, args)
        if args.selector:
            data = page.locator(args.selector).screenshot()
        else:
            data = page.screenshot(full_page=args.full_page)
    _write_bytes(args.output, data)
    return 0


def cmd_pdf(args: argparse.Namespace) -> int:
    if args.session:
        if args.url:
            sys.exit("web pdf: positional URL not allowed with --session")
        if args.output == "-":
            data_b64 = _session_call(
                args.session,
                {
                    "op": "pdf",
                    "inline": True,
                    "timeout": args.timeout,
                },
            )
            sys.stdout.buffer.write(base64.b64decode(data_b64))
        else:
            out = str(Path(args.output).resolve())
            _session_call(
                args.session,
                {
                    "op": "pdf",
                    "path": out,
                    "timeout": args.timeout,
                },
            )
        return 0
    url = _require_url("pdf", args.url)
    with _open_page_oneshot(args) as page:
        _navigate_oneshot(page, url, args)
        data = page.pdf()
    _write_bytes(args.output, data)
    return 0


def cmd_eval(args: argparse.Namespace) -> int:
    if args.session:
        if args.second is not None:
            sys.exit("web eval: only the JS expression is positional with --session")
        result = _session_call(
            args.session,
            {
                "op": "eval",
                "expression": args.first,
                "timeout": args.timeout,
            },
        )
        _emit_json(result)
        return 0
    if args.second is None:
        sys.exit("web eval: URL and JS expression required without --session")
    url, expression = args.first, args.second
    with _open_page_oneshot(args) as page:
        _navigate_oneshot(page, url, args)
        result = page.evaluate(f"() => ({expression})")
    _emit_json(result)
    return 0


def cmd_links(args: argparse.Namespace) -> int:
    js = (
        "Array.from(document.querySelectorAll('a[href]'))"
        ".map(a => ({text: (a.innerText||'').trim(), href: a.href}))"
    )
    if args.session:
        if args.url:
            sys.exit("web links: positional URL not allowed with --session")
        result = _session_call(
            args.session,
            {
                "op": "eval",
                "expression": js,
                "timeout": args.timeout,
            },
        )
        _emit_json(result)
        return 0
    url = _require_url("links", args.url)
    with _open_page_oneshot(args) as page:
        _navigate_oneshot(page, url, args)
        links = page.evaluate(f"() => ({js})")
    _emit_json(links)
    return 0


####
## One-shot-only commands


def cmd_console(args: argparse.Namespace) -> int:
    entries: list[dict[str, str]] = []
    with _open_page_oneshot(args) as page:
        page.on(
            "console", lambda msg: entries.append({"type": msg.type, "text": msg.text})
        )
        page.on(
            "pageerror",
            lambda err: entries.append({"type": "pageerror", "text": str(err)}),
        )
        _navigate_oneshot(page, args.url, args)
    for entry in entries:
        _emit_json(entry)
    return 0


def cmd_network(args: argparse.Namespace) -> int:
    entries: list[dict[str, Any]] = []

    def on_response(resp: Response) -> None:
        try:
            length = int(resp.header_value("content-length") or 0)
        except (TypeError, ValueError):
            length = 0
        mime = (resp.headers.get("content-type") or "").split(";", 1)[0].strip()
        entries.append(
            {
                "url": resp.url,
                "method": resp.request.method,
                "status": resp.status,
                "mimeType": mime,
                "sizeBytes": length,
                "resourceType": resp.request.resource_type,
            }
        )

    with _open_page_oneshot(args) as page:
        page.on("response", on_response)
        _navigate_oneshot(page, args.url, args)
    for entry in entries:
        _emit_json(entry)
    return 0


####
## Session paths


def _session_dir() -> Path:
    base = os.environ.get("XDG_RUNTIME_DIR")
    if not base:
        base = f"/tmp/web-cli-{os.getuid()}"
    p = Path(base) / "web-cli"
    p.mkdir(parents=True, exist_ok=True)
    with suppress(OSError):
        p.chmod(0o700)
    return p


def _session_paths(name: str) -> dict[str, Path]:
    d = _session_dir()
    return {
        "sock": d / f"{name}.sock",
        "pid": d / f"{name}.pid",
        "meta": d / f"{name}.json",
        "log": d / f"{name}.log",
    }


def _is_pid_alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def _read_pid(paths: dict[str, Path]) -> int | None:
    try:
        return int(paths["pid"].read_text().strip())
    except (OSError, ValueError):
        return None


def _cleanup_stale(paths: dict[str, Path]) -> None:
    for key in ("sock", "pid", "meta"):
        with suppress(FileNotFoundError):
            paths[key].unlink()


####
## Session client


def _session_call(name: str, op: dict[str, Any], timeout_s: float = 60.0) -> Any:
    paths = _session_paths(name)
    if not paths["sock"].exists():
        sys.exit(
            f"web: no session named {name!r} (start with `web session start --name {name}`)"
        )
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.settimeout(timeout_s)
    try:
        sock.connect(str(paths["sock"]))
    except (FileNotFoundError, ConnectionRefusedError):
        sys.exit(
            f"web: session {name!r} is not responding (try `web session stop --name {name}`)"
        )
    try:
        sock.sendall((json.dumps(op) + "\n").encode())
        chunks: list[bytes] = []
        while True:
            buf = sock.recv(65536)
            if not buf:
                break
            chunks.append(buf)
            if buf.endswith(b"\n") and b"\n" in b"".join(chunks):
                break
        line = b"".join(chunks).split(b"\n", 1)[0]
    finally:
        sock.close()
    resp = json.loads(line)
    if not resp.get("ok"):
        sys.exit(f"web: {resp.get('error', 'unknown error')}")
    return resp.get("result")


####
## Session lifecycle


def cmd_session_start(args: argparse.Namespace) -> int:
    paths = _session_paths(args.name)
    pid = _read_pid(paths)
    if pid is not None and _is_pid_alive(pid):
        print(
            f"web: session {args.name!r} already running (pid {pid})", file=sys.stderr
        )
        return 1
    _cleanup_stale(paths)

    cmd = [
        sys.executable,
        sys.argv[0],
        "__daemon__",
        "--name",
        args.name,
        "--viewport",
        f"{args.viewport['width']}x{args.viewport['height']}",
    ]
    if args.headed:
        cmd.append("--headed")
    if args.user_agent:
        cmd += ["--user-agent", args.user_agent]
    if args.state_file:
        cmd += ["--state-file", str(Path(args.state_file).resolve())]

    with paths["log"].open("wb") as log:
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
            print(args.name)
            return 0
        if proc.poll() is not None:
            print(
                f"web: daemon exited before becoming ready; see {paths['log']}",
                file=sys.stderr,
            )
            return 1
        time.sleep(0.1)
    print(f"web: timed out waiting for session {args.name!r}", file=sys.stderr)
    proc.terminate()
    return 1


def cmd_session_stop(args: argparse.Namespace) -> int:
    paths = _session_paths(args.name)
    pid = _read_pid(paths)
    if pid is None:
        _cleanup_stale(paths)
        if not args.quiet:
            print(f"web: no session named {args.name!r}", file=sys.stderr)
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
        try:
            current = _session_call(name, {"op": "url"}, timeout_s=2.0)
            meta["url"] = current.get("url")
            meta["title"] = current.get("title")
        except SystemExit:
            pass
        sessions.append(meta)
    for entry in sessions:
        _emit_json(entry)
    return 0


def cmd_session_save_state(args: argparse.Namespace) -> int:
    out = str(Path(args.output).resolve())
    _session_call(args.session, {"op": "save_state", "path": out})
    return 0


####
## Session-only page actions


def cmd_nav(args: argparse.Namespace) -> int:
    info = _session_call(
        args.session,
        {
            "op": "nav",
            "url": args.url,
            "wait_until": args.wait_until,
            "wait_for": args.wait_for,
            "timeout": args.timeout,
        },
    )
    _emit_json(info)
    return 0


def cmd_click(args: argparse.Namespace) -> int:
    _session_call(
        args.session,
        {
            "op": "click",
            "selector": args.selector,
            "timeout": args.timeout,
            "no_wait_after": args.no_wait_after,
            "force": args.force,
        },
    )
    return 0


def cmd_fill(args: argparse.Namespace) -> int:
    _session_call(
        args.session,
        {
            "op": "fill",
            "selector": args.selector,
            "value": args.value,
            "timeout": args.timeout,
            "no_wait_after": args.no_wait_after,
        },
    )
    return 0


def cmd_press(args: argparse.Namespace) -> int:
    _session_call(
        args.session,
        {
            "op": "press",
            "selector": args.selector,
            "key": args.key,
            "timeout": args.timeout,
            "no_wait_after": args.no_wait_after,
        },
    )
    return 0


def cmd_wait(args: argparse.Namespace) -> int:
    _session_call(
        args.session,
        {
            "op": "wait",
            "selector": args.selector,
            "state": args.state,
            "timeout": args.timeout,
        },
    )
    return 0


def cmd_back(args: argparse.Namespace) -> int:
    _session_call(args.session, {"op": "back", "timeout": args.timeout})
    return 0


def cmd_forward(args: argparse.Namespace) -> int:
    _session_call(args.session, {"op": "forward", "timeout": args.timeout})
    return 0


def cmd_reload(args: argparse.Namespace) -> int:
    _session_call(args.session, {"op": "reload", "timeout": args.timeout})
    return 0


def cmd_url(args: argparse.Namespace) -> int:
    info = _session_call(args.session, {"op": "url"})
    _emit_json(info)
    return 0


####
## Daemon


async def _daemon_main(args: argparse.Namespace) -> int:
    from playwright.async_api import async_playwright

    paths = _session_paths(args.name)
    paths["pid"].write_text(str(os.getpid()))
    paths["meta"].write_text(
        json.dumps(
            {
                "name": args.name,
                "pid": os.getpid(),
                "started": time.time(),
                "headless": not args.headed,
                "viewport": args.viewport,
            }
        )
    )

    if paths["sock"].exists():
        paths["sock"].unlink()

    stop_event = asyncio.Event()
    loop = asyncio.get_running_loop()
    for sig in (signal.SIGTERM, signal.SIGINT):
        loop.add_signal_handler(sig, stop_event.set)

    async with async_playwright() as pw:
        browser = await pw.chromium.launch(
            headless=not args.headed,
            channel="chromium",
            args=["--disable-dev-shm-usage"],
        )
        ctx_kwargs: dict[str, Any] = {"viewport": args.viewport}
        if args.user_agent:
            ctx_kwargs["user_agent"] = args.user_agent
        if args.state_file:
            ctx_kwargs["storage_state"] = args.state_file
        context = await browser.new_context(**ctx_kwargs)
        page = await context.new_page()
        lock = asyncio.Lock()

        async def handle(
            reader: asyncio.StreamReader, writer: asyncio.StreamWriter
        ) -> None:
            try:
                line = await reader.readline()
                if not line:
                    return
                req = json.loads(line)
                async with lock:
                    try:
                        result = await _dispatch_guarded(page, context, req)
                        resp: dict[str, Any] = {"ok": True, "result": result}
                    except Exception as exc:
                        resp = {"ok": False, "error": f"{type(exc).__name__}: {exc}"}
                writer.write((json.dumps(resp, default=str) + "\n").encode())
                await writer.drain()
            except Exception:
                pass
            finally:
                writer.close()
                with suppress(Exception):
                    await writer.wait_closed()

        server = await asyncio.start_unix_server(handle, str(paths["sock"]))
        with suppress(OSError):
            paths["sock"].chmod(0o600)

        try:
            await stop_event.wait()
        finally:
            server.close()
            await server.wait_closed()
            await context.close()
            await browser.close()
            _cleanup_stale(paths)
    return 0


async def _dispatch_guarded(page: Any, context: Any, req: dict[str, Any]) -> Any:
    op_timeout = int(req.get("timeout", 30000))
    deadline_s = (op_timeout + 5000) / 1000.0
    try:
        return await asyncio.wait_for(_dispatch(page, context, req), timeout=deadline_s)
    except asyncio.TimeoutError as exc:
        raise TimeoutError(
            f"op {req.get('op')!r} exceeded wall-clock deadline "
            f"({op_timeout + 5000}ms); session may need restart"
        ) from exc


async def _dispatch(page: Any, context: Any, req: dict[str, Any]) -> Any:
    op = req.get("op")
    timeout = req.get("timeout", 30000)
    if op == "nav":
        await page.goto(
            req["url"],
            wait_until=req.get("wait_until") or "domcontentloaded",
            timeout=timeout,
        )
        if req.get("wait_for"):
            await page.wait_for_selector(req["wait_for"], timeout=timeout)
        return {"url": page.url, "title": await page.title()}
    if op == "click":
        kwargs: dict[str, Any] = {"timeout": timeout}
        if req.get("no_wait_after"):
            kwargs["no_wait_after"] = True
        if req.get("force"):
            kwargs["force"] = True
        await page.locator(req["selector"]).click(**kwargs)
        return {}
    if op == "fill":
        kwargs = {"timeout": timeout}
        if req.get("no_wait_after"):
            kwargs["no_wait_after"] = True
        await page.locator(req["selector"]).fill(req["value"], **kwargs)
        return {}
    if op == "press":
        kwargs = {"timeout": timeout}
        if req.get("no_wait_after"):
            kwargs["no_wait_after"] = True
        await page.locator(req["selector"]).press(req["key"], **kwargs)
        return {}
    if op == "wait":
        await page.wait_for_selector(
            req["selector"],
            state=req.get("state") or "visible",
            timeout=timeout,
        )
        return {}
    if op == "eval":
        return await page.evaluate(f"() => ({req['expression']})")
    if op == "text":
        sel = req.get("selector") or "body"
        return await page.inner_text(sel, timeout=timeout)
    if op == "html":
        sel = req.get("selector")
        if sel:
            return await page.locator(sel).evaluate("el => el.outerHTML")
        return await page.content()
    if op == "screenshot":
        kwargs = {}
        if req.get("path"):
            kwargs["path"] = req["path"]
        if req.get("selector"):
            data = await page.locator(req["selector"]).screenshot(**kwargs)
        else:
            kwargs["full_page"] = bool(req.get("full_page"))
            data = await page.screenshot(**kwargs)
        if req.get("inline"):
            return base64.b64encode(data).decode("ascii")
        return {"path": req.get("path")}
    if op == "pdf":
        kwargs = {}
        if req.get("path"):
            kwargs["path"] = req["path"]
        data = await page.pdf(**kwargs)
        if req.get("inline"):
            return base64.b64encode(data).decode("ascii")
        return {"path": req.get("path")}
    if op == "back":
        await page.go_back(timeout=timeout)
        return {}
    if op == "forward":
        await page.go_forward(timeout=timeout)
        return {}
    if op == "reload":
        await page.reload(timeout=timeout)
        return {}
    if op == "url":
        return {"url": page.url, "title": await page.title()}
    if op == "save_state":
        await context.storage_state(path=req["path"])
        return {"path": req["path"]}
    raise ValueError(f"unknown op: {op}")


def cmd_daemon(args: argparse.Namespace) -> int:
    return asyncio.run(_daemon_main(args))


####
## Argument parser


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="web", description=__doc__)
    sub = parser.add_subparsers(dest="cmd", required=True)

    # Bimodal: take URL when one-shot, attach to --session otherwise.
    p = sub.add_parser(
        "html", help="rendered HTML (one-shot URL or --session [SELECTOR])"
    )
    _add_bimodal_common(p)
    p.add_argument(
        "target",
        nargs="?",
        default=None,
        help="URL (one-shot) or CSS selector (session, default: whole page)",
    )
    p.set_defaults(func=cmd_html)

    p = sub.add_parser(
        "text", help="rendered text (one-shot URL or --session [SELECTOR])"
    )
    _add_bimodal_common(p)
    p.add_argument(
        "target",
        nargs="?",
        default=None,
        help="URL (one-shot) or CSS selector (session, default: 'body')",
    )
    p.set_defaults(func=cmd_text)

    p = sub.add_parser("screenshot", help="PNG screenshot (one-shot URL or --session)")
    _add_bimodal_common(p)
    p.add_argument("url", nargs="?", default=None, help="URL (omit with --session)")
    p.add_argument("-o", "--output", required=True, help='path or "-" for stdout')
    p.add_argument("--full-page", action="store_true")
    p.add_argument("--selector", default=None, help="screenshot only matching element")
    p.set_defaults(func=cmd_screenshot)

    p = sub.add_parser("pdf", help="render to PDF (one-shot URL or --session)")
    _add_bimodal_common(p)
    p.add_argument("url", nargs="?", default=None, help="URL (omit with --session)")
    p.add_argument("-o", "--output", required=True, help='path or "-" for stdout')
    p.set_defaults(func=cmd_pdf)

    p = sub.add_parser(
        "eval", help="run JS expression (one-shot URL+EXPR or --session EXPR)"
    )
    _add_bimodal_common(p)
    p.add_argument("first", help="URL (one-shot) or JS expression (session)")
    p.add_argument(
        "second",
        nargs="?",
        default=None,
        help="JS expression (one-shot only)",
    )
    p.set_defaults(func=cmd_eval)

    p = sub.add_parser("links", help="extract anchors as JSON array")
    _add_bimodal_common(p)
    p.add_argument("url", nargs="?", default=None, help="URL (omit with --session)")
    p.set_defaults(func=cmd_links)

    # One-shot only
    p = sub.add_parser(
        "console", help="capture console.* and pageerror during load (JSONL)"
    )
    _add_bimodal_common(p)
    p.add_argument("url")
    p.set_defaults(func=cmd_console)

    p = sub.add_parser("network", help="log network responses for a URL (JSONL)")
    _add_bimodal_common(p)
    p.add_argument("url")
    p.set_defaults(func=cmd_network)

    # Session lifecycle
    sess = sub.add_parser("session", help="manage long-lived browser sessions")
    sess_sub = sess.add_subparsers(dest="session_cmd", required=True)

    sp = sess_sub.add_parser("start", help="start a long-lived browser session")
    sp.add_argument("--name", default="default")
    sp.add_argument(
        "--headed",
        action="store_true",
        help="run with a visible window (needs $DISPLAY)",
    )
    sp.add_argument(
        "--viewport",
        type=_viewport,
        default={"width": 1280, "height": 800},
        metavar="WxH",
    )
    sp.add_argument("--user-agent", default=None)
    sp.add_argument(
        "--state-file", default=None, help="load Playwright storage state from file"
    )
    sp.set_defaults(func=cmd_session_start)

    sp = sess_sub.add_parser("stop", help="stop a session")
    sp.add_argument("--name", default="default")
    sp.add_argument(
        "--quiet", action="store_true", help="exit 0 even if session was missing"
    )
    sp.set_defaults(func=cmd_session_stop)

    sp = sess_sub.add_parser("list", help="list active sessions (JSONL)")
    sp.set_defaults(func=cmd_session_list)

    sp = sess_sub.add_parser("save-state", help="dump cookies/localStorage to a file")
    _add_session_arg(sp)
    sp.add_argument("-o", "--output", required=True)
    sp.set_defaults(func=cmd_session_save_state)

    # Session-only page actions
    p = sub.add_parser("nav", help="navigate to a URL in a session")
    _add_session_arg(p)
    p.add_argument("url")
    p.add_argument(
        "--wait-until",
        choices=("domcontentloaded", "load", "networkidle", "commit"),
        default="domcontentloaded",
    )
    p.add_argument("--wait-for", metavar="SELECTOR", default=None)
    p.set_defaults(func=cmd_nav)

    p = sub.add_parser("click", help="click an element matching selector")
    _add_session_arg(p)
    p.add_argument("selector")
    p.add_argument(
        "--no-wait-after",
        action="store_true",
        help="don't wait for any post-click navigation",
    )
    p.add_argument(
        "--force",
        action="store_true",
        help="skip actionability checks (visible/stable/etc.)",
    )
    p.set_defaults(func=cmd_click)

    p = sub.add_parser("fill", help="fill an input")
    _add_session_arg(p)
    p.add_argument("selector")
    p.add_argument("value")
    p.add_argument("--no-wait-after", action="store_true")
    p.set_defaults(func=cmd_fill)

    p = sub.add_parser("press", help="press a key on an element (e.g. Enter)")
    _add_session_arg(p)
    p.add_argument("selector")
    p.add_argument("key")
    p.add_argument("--no-wait-after", action="store_true")
    p.set_defaults(func=cmd_press)

    p = sub.add_parser("wait", help="wait for selector to reach state")
    _add_session_arg(p)
    p.add_argument("selector")
    p.add_argument(
        "--state",
        choices=("attached", "detached", "visible", "hidden"),
        default="visible",
    )
    p.set_defaults(func=cmd_wait)

    p = sub.add_parser("back", help="navigate back")
    _add_session_arg(p)
    p.set_defaults(func=cmd_back)

    p = sub.add_parser("forward", help="navigate forward")
    _add_session_arg(p)
    p.set_defaults(func=cmd_forward)

    p = sub.add_parser("reload", help="reload current page")
    _add_session_arg(p)
    p.set_defaults(func=cmd_reload)

    p = sub.add_parser("url", help="current URL and title")
    _add_session_arg(p)
    p.set_defaults(func=cmd_url)

    # Hidden daemon entrypoint
    d = sub.add_parser("__daemon__")
    d.add_argument("--name", required=True)
    d.add_argument("--headed", action="store_true")
    d.add_argument("--viewport", type=_viewport, default={"width": 1280, "height": 800})
    d.add_argument("--user-agent", default=None)
    d.add_argument("--state-file", default=None)
    d.set_defaults(func=cmd_daemon)

    return parser


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        rc = args.func(args)
    except PlaywrightTimeoutError as exc:
        print(f"web: timeout: {exc}", file=sys.stderr)
        return 124
    except KeyboardInterrupt:
        return 130
    return rc if isinstance(rc, int) else 0


if __name__ == "__main__":
    sys.exit(main())

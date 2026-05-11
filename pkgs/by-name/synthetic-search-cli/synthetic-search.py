"""synthetic-search — web search via the Synthetic Search API."""

from __future__ import annotations

import argparse
import json
import os
import socket
import sys
import urllib.error
import urllib.request
from typing import Any

_API_URL = "https://api.synthetic.new/v2/search"


def _get_api_key() -> str:
    key = os.environ.get("SYNTHETIC_API_KEY", "")
    if not key:
        print("synthetic-search: SYNTHETIC_API_KEY is not set", file=sys.stderr)
        sys.exit(1)
    return key


def _emit(data: Any, *, raw_bytes: bytes | None, raw: bool, compact: bool) -> None:
    if raw and raw_bytes is not None:
        sys.stdout.buffer.write(raw_bytes)
        if not raw_bytes.endswith(b"\n"):
            sys.stdout.buffer.write(b"\n")
        return
    if compact:
        print(json.dumps(data, separators=(",", ":")))
    else:
        print(json.dumps(data, indent=2))


def cmd_search(args: argparse.Namespace) -> int:
    key = _get_api_key()
    payload = json.dumps({"query": args.query}).encode("utf-8")
    req = urllib.request.Request(
        _API_URL,
        data=payload,
        headers={
            "Authorization": f"Bearer {key}",
            "Content-Type": "application/json",
        },
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=args.timeout) as resp:
            raw_bytes = resp.read()
    except urllib.error.HTTPError as exc:
        body = exc.read().decode("utf-8", errors="replace")
        print(
            f"synthetic-search: HTTP {exc.code}: {body}",
            file=sys.stderr,
        )
        return 1
    except urllib.error.URLError as exc:
        if isinstance(exc.reason, socket.timeout):
            print("synthetic-search: request timed out", file=sys.stderr)
            return 124
        print(f"synthetic-search: request failed: {exc.reason}", file=sys.stderr)
        return 1
    except socket.timeout:
        print("synthetic-search: request timed out", file=sys.stderr)
        return 124

    try:
        data = json.loads(raw_bytes)
    except json.JSONDecodeError as exc:
        print(f"synthetic-search: invalid JSON response: {exc}", file=sys.stderr)
        return 1

    _emit(data, raw_bytes=raw_bytes, raw=args.raw, compact=args.compact)
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="synthetic-search",
        description=__doc__,
    )
    sub = parser.add_subparsers(dest="cmd", required=True)

    p = sub.add_parser("search", help="search the web (default subcommand)")
    p.add_argument("query", help="search query")
    p.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        metavar="SECONDS",
        help="request timeout in seconds (default: 30)",
    )
    p.add_argument(
        "--raw",
        action="store_true",
        help="output raw API response bytes without decoding",
    )
    p.add_argument(
        "--compact",
        action="store_true",
        help="compact JSON output (no whitespace)",
    )
    p.set_defaults(func=cmd_search)

    return parser


def main(argv: list[str] | None = None) -> int:
    if argv is None:
        argv = sys.argv[1:]
    # Default subcommand: treat bare args as `search QUERY`
    known_subcommands = {"search"}
    if not argv or argv[0] not in known_subcommands:
        argv = ["search"] + list(argv)
    args = build_parser().parse_args(argv)
    try:
        rc = args.func(args)
    except KeyboardInterrupt:
        return 130
    return rc if isinstance(rc, int) else 0


if __name__ == "__main__":
    sys.exit(main())

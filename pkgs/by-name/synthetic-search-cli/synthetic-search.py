"""synthetic-search — web search via the Synthetic Search API."""

from __future__ import annotations

import argparse
import json
import os
import socket
import sys
import textwrap
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


def _do_search(query: str, *, timeout: float) -> Any:
    """POST a query and return parsed JSON, or sys.exit on error."""
    key = _get_api_key()
    payload = json.dumps({"query": query}).encode("utf-8")
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
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            raw_bytes = resp.read()
    except urllib.error.HTTPError as exc:
        body = exc.read().decode("utf-8", errors="replace")
        print(
            f"synthetic-search: HTTP {exc.code}: {body}",
            file=sys.stderr,
        )
        sys.exit(1)
    except urllib.error.URLError as exc:
        if isinstance(exc.reason, socket.timeout):
            print("synthetic-search: request timed out", file=sys.stderr)
            sys.exit(124)
        print(f"synthetic-search: request failed: {exc.reason}", file=sys.stderr)
        sys.exit(1)
    except socket.timeout:
        print("synthetic-search: request timed out", file=sys.stderr)
        sys.exit(124)

    try:
        return json.loads(raw_bytes)
    except json.JSONDecodeError as exc:
        print(f"synthetic-search: invalid JSON response: {exc}", file=sys.stderr)
        sys.exit(1)


# --- Output modes -----------------------------------------------------------

def _format_json(data: Any, *, compact: bool) -> None:
    if compact:
        print(json.dumps(data, separators=(",", ":")))
    else:
        print(json.dumps(data, indent=2))


def _format_list(data: Any) -> None:
    for r in data.get("results", []):
        print(f"{r.get('title', '')}: {r.get('url', '')}")


def _format_text(data: Any) -> None:
    for i, r in enumerate(data.get("results", []), 1):
        title = r.get("title", "")
        url = r.get("url", "")
        snippet = r.get("text", "")
        published = r.get("published", "")
        meta = f"  published: {published}" if published else ""
        print(f"[{i}] {title}")
        print(f"  url: {url}{meta}")
        if snippet:
            for line in textwrap.wrap(snippet, width=80, initial_indent="  ", subsequent_indent="  "):
                print(line)
        print()


# --- CLI ---------------------------------------------------------------------

def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="synthetic-search",
        description=__doc__,
    )
    parser.add_argument("query", help="search query")
    parser.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        metavar="SECONDS",
        help="request timeout in seconds (default: 30)",
    )
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--compact",
        action="store_true",
        help="compact JSON output (no whitespace)",
    )
    group.add_argument(
        "--list",
        action="store_true",
        help="output title and URL per line",
    )
    group.add_argument(
        "--text",
        action="store_true",
        help="output readable results with snippets",
    )
    return parser


def main(argv: list[str] | None = None) -> int:
    if argv is None:
        argv = sys.argv[1:]
    args = build_parser().parse_args(argv)
    try:
        data = _do_search(args.query, timeout=args.timeout)
    except KeyboardInterrupt:
        return 130
    if args.list:
        _format_list(data)
    elif args.text:
        _format_text(data)
    else:
        _format_json(data, compact=args.compact)
    return 0


if __name__ == "__main__":
    sys.exit(main())

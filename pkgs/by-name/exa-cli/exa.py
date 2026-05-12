"""exa — search, extract, and discover with the Exa AI API."""

from __future__ import annotations

import argparse
import json
import os
import socket
import sys
import time
import urllib.error
import urllib.request
from typing import Any

_API_BASE = "https://api.exa.ai"


####
## Shared helpers


def _get_api_key() -> str:
    key = os.environ.get("EXA_API_KEY", "")
    if not key:
        print("exa: EXA_API_KEY is not set", file=sys.stderr)
        sys.exit(1)
    return key


def _request(
    method: str,
    path: str,
    body: Any = None,
    *,
    params: dict[str, str] | None = None,
    timeout: float = 60.0,
) -> Any:
    """Send a request to the Exa API, return parsed JSON or exit on error."""
    key = _get_api_key()
    data = json.dumps(body).encode("utf-8") if body is not None else None

    qs = ""
    if params:
        qs = "?" + "&".join(f"{k}={v}" for k, v in params.items())

    req = urllib.request.Request(
        f"{_API_BASE}{path}{qs}",
        data=data,
        headers={
            "User-Agent": "curl/7.54.1",
            "x-api-key": key,
            "Content-Type": "application/json",
            "Accept": "application/json",
        },
        method=method,
    )
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return json.loads(resp.read())
    except urllib.error.HTTPError as exc:
        err_body = exc.read().decode("utf-8", errors="replace")
        print(f"exa: HTTP {exc.code}: {err_body}", file=sys.stderr)
        sys.exit(1)
    except urllib.error.URLError as exc:
        if isinstance(exc.reason, socket.timeout):
            print("exa: request timed out", file=sys.stderr)
            sys.exit(124)
        print(f"exa: request failed: {exc.reason}", file=sys.stderr)
        sys.exit(1)


def _format_json(data: Any, *, compact: bool) -> None:
    if compact:
        print(json.dumps(data, separators=(",", ":")))
    else:
        print(json.dumps(data, indent=2))


def _add_output_args(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--output",
        choices=["json", "compact", "text"],
        default="json",
        help="output format (default: json)",
    )


####
## search


def _format_search_text(data: Any) -> None:
    results = data.get("results", [])
    if not results:
        print("(no results)")
        return
    for i, r in enumerate(results, 1):
        title = r.get("title", "(untitled)")
        url = r.get("url", "")
        pub = r.get("publishedDate", "")
        print(f"## {i}. {title}")
        print(f"   {url}")
        if pub:
            print(f"   published: {pub}")
        if r.get("highlights"):
            for h in r["highlights"]:
                print(f"   > {h}")
        elif r.get("text"):
            text = r["text"]
            preview = text[:500] + "…" if len(text) > 500 else text
            print(f"   {preview}")
        if r.get("summary"):
            print(f"   summary: {r['summary']}")
        print()
    cost = data.get("costDollars", {}).get("total")
    if cost is not None:
        print(f"(cost: ${cost})", file=sys.stderr)


def cmd_search(args: argparse.Namespace) -> int:
    payload: dict[str, Any] = {"query": args.query}

    if args.type:
        payload["type"] = args.type
    if args.num_results is not None:
        payload["numResults"] = args.num_results
    if args.category:
        payload["category"] = args.category
    if args.include_domains:
        payload["includeDomains"] = args.include_domains
    if args.exclude_domains:
        payload["excludeDomains"] = args.exclude_domains
    if args.start_published_date:
        payload["startPublishedDate"] = args.start_published_date
    if args.end_published_date:
        payload["endPublishedDate"] = args.end_published_date
    if args.moderation:
        payload["moderation"] = args.moderation

    contents: dict[str, Any] = {}
    if args.highlights:
        contents["highlights"] = True
    if args.summary:
        contents["summary"] = True
    if args.text:
        contents["text"] = True
    if args.max_characters is not None:
        contents.setdefault("text", True)
        contents["text"] = {"maxCharacters": args.max_characters}
    if args.max_age_hours is not None:
        contents["maxAgeHours"] = args.max_age_hours

    if contents:
        payload["contents"] = contents

    data = _request("POST", "/search", payload, timeout=args.timeout)

    if args.output == "text":
        _format_search_text(data)
    else:
        _format_json(data, compact=(args.output == "compact"))
    return 0


####
## contents


def _format_contents_text(data: Any) -> None:
    results = data.get("results", [])
    if not results:
        print("(no results)")
        return
    for i, r in enumerate(results, 1):
        title = r.get("title", "(untitled)")
        url = r.get("url", "")
        print(f"## {i}. {title}")
        print(f"   {url}")
        if r.get("highlights"):
            for h in r["highlights"]:
                print(f"   > {h}")
        elif r.get("text"):
            text = r["text"]
            preview = text[:500] + "…" if len(text) > 500 else text
            print(f"   {preview}")
        if r.get("summary"):
            print(f"   summary: {r['summary']}")
        print()


def cmd_contents(args: argparse.Namespace) -> int:
    payload: dict[str, Any] = {"urls": args.urls}

    if args.text:
        payload["text"] = True
    if args.highlights:
        payload["highlights"] = True
    if args.summary:
        payload["summary"] = True
    if args.max_age_hours is not None:
        payload["maxAgeHours"] = args.max_age_hours
    if args.livecrawl_timeout is not None:
        payload["livecrawlTimeout"] = args.livecrawl_timeout

    data = _request("POST", "/contents", payload, timeout=args.timeout)

    if args.output == "text":
        _format_contents_text(data)
    else:
        _format_json(data, compact=(args.output == "compact"))
    return 0


####
## context


def cmd_context(args: argparse.Namespace) -> int:
    tokens: Any = args.tokens
    if isinstance(tokens, str) and tokens != "dynamic":
        try:
            tokens = int(tokens)
        except ValueError:
            print('exa: --tokens must be an integer or "dynamic"', file=sys.stderr)
            return 2

    payload: dict[str, Any] = {"query": args.query, "tokensNum": tokens}
    data = _request("POST", "/context", payload, timeout=args.timeout)

    if args.output == "text":
        response = data.get("response", data)
        if isinstance(response, str):
            print(response)
        else:
            _format_json(response, compact=(args.output == "compact"))
    else:
        _format_json(data, compact=(args.output == "compact"))
    return 0


####
## websets


def _websets_path(*parts: str) -> str:
    return "/websets/v0/websets/" + "/".join(parts)


def cmd_websets_create(args: argparse.Namespace) -> int:
    if args.file:
        body = json.loads(args.file.read())
    else:
        body = json.loads(args.payload)
    data = _request("POST", _websets_path(), body, timeout=args.timeout)

    if args.output == "compact":
        _format_json(data, compact=True)
    elif args.output == "text":
        _format_webset_text(data)
    else:
        _format_json(data, compact=False)
    return 0


def cmd_websets_get(args: argparse.Namespace) -> int:
    params: dict[str, str] = {}
    if args.expand:
        params["expand"] = args.expand
    data = _request("GET", _websets_path(args.id), params=params, timeout=args.timeout)

    if args.output == "compact":
        _format_json(data, compact=True)
    elif args.output == "text":
        _format_webset_text(data)
    else:
        _format_json(data, compact=False)
    return 0


def cmd_websets_list(args: argparse.Namespace) -> int:
    params: dict[str, str] = {}
    if args.cursor:
        params["cursor"] = args.cursor
    if args.limit is not None:
        params["limit"] = str(args.limit)
    data = _request("GET", _websets_path(), params=params, timeout=args.timeout)

    if args.output == "compact":
        _format_json(data, compact=True)
    elif args.output == "text":
        for ws in data.get("data", []):
            _format_webset_text(ws)
    else:
        _format_json(data, compact=False)
    return 0


def cmd_websets_items(args: argparse.Namespace) -> int:
    params: dict[str, str] = {}
    if args.cursor:
        params["cursor"] = args.cursor
    if args.limit is not None:
        params["limit"] = str(args.limit)
    data = _request(
        "GET",
        _websets_path(args.webset_id, "items"),
        params=params,
        timeout=args.timeout,
    )

    if args.output == "compact":
        _format_json(data, compact=True)
    elif args.output == "text":
        _format_items_text(data)
    else:
        _format_json(data, compact=False)
    return 0


def cmd_websets_delete(args: argparse.Namespace) -> int:
    if not args.force:
        print("exa websets delete: use --force to confirm deletion", file=sys.stderr)
        return 1
    data = _request("DELETE", _websets_path(args.id), timeout=args.timeout)
    _format_json(data, compact=(args.output == "compact"))
    return 0


def cmd_websets_wait(args: argparse.Namespace) -> int:
    deadline = time.monotonic() + args.timeout
    interval = args.interval
    while True:
        data = _request("GET", _websets_path(args.id), timeout=args.timeout)
        status = data.get("status", "unknown")
        if status == "idle":
            if args.output == "compact":
                _format_json(data, compact=True)
            elif args.output == "text":
                _format_webset_text(data)
            else:
                _format_json(data, compact=False)
            return 0
        if time.monotonic() >= deadline:
            print(
                f"exa websets wait: timed out after {args.timeout}s (status: {status})",
                file=sys.stderr,
            )
            return 124
        print(f"  status: {status} …", file=sys.stderr)
        time.sleep(interval)


def cmd_websets_cancel(args: argparse.Namespace) -> int:
    data = _request(
        "POST", _websets_path(args.id, "cancel"), timeout=args.timeout
    )
    _format_json(data, compact=(args.output == "compact"))
    return 0


def _format_webset_text(ws: Any) -> None:
    ws_id = ws.get("id", "?")
    status = ws.get("status", "?")
    searches = ws.get("searches", [])
    enrichments = ws.get("enrichments", [])
    print(f"webset {ws_id}  status={status}")
    for s in searches:
        q = s.get("query", "?")
        prog = s.get("progress", {})
        found = prog.get("found", "?")
        pct = prog.get("completion", "?")
        print(f"  search: {q}  found={found}  completion={pct}%")
    for e in enrichments:
        desc = e.get("description", "?")
        fmt = e.get("format", "?")
        print(f"  enrichment: {desc} ({fmt})")
    items = ws.get("items")
    if items:
        print(f"  items: {len(items)}")
    print()


def _format_items_text(data: Any) -> None:
    items = data.get("data", [])
    if not items:
        print("(no items)")
        return
    for i, item in enumerate(items, 1):
        props = item.get("properties", {})
        url = props.get("url", "?")
        itype = props.get("type", "?")
        name = ""
        if itype == "company" and props.get("company"):
            name = props["company"].get("name", "")
        elif itype == "person" and props.get("person"):
            name = props["person"].get("name", "")
        label = f"{name} — " if name else ""
        print(f"{i}. {label}{url}")
        for enr in item.get("enrichments", []):
            desc = enr.get("description", "?")
            result = enr.get("result")
            if result:
                print(f"   {desc}: {', '.join(str(v) for v in result)}")
        print()


####
## Argument parsing


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="exa", description=__doc__)
    sub = parser.add_subparsers(dest="cmd", required=True)

    # -- search -----------------------------------------------------------
    p = sub.add_parser("search", help="neural search over the web")
    p.add_argument("query", help="natural language search query")
    p.add_argument(
        "--type",
        choices=(
            "auto",
            "fast",
            "instant",
            "deep-lite",
            "deep",
            "deep-reasoning",
        ),
        default=None,
        help="search type (default: auto)",
    )
    p.add_argument(
        "--num-results", type=int, default=None, metavar="N", help="max results (default 10)"
    )
    p.add_argument("--category", default=None, help="category filter (news, company, …)")
    p.add_argument(
        "--include-domains",
        action="append",
        default=None,
        metavar="DOMAIN",
        help="restrict to domain; repeatable",
    )
    p.add_argument(
        "--exclude-domains",
        action="append",
        default=None,
        metavar="DOMAIN",
        help="exclude domain; repeatable",
    )
    p.add_argument(
        "--start-published-date",
        default=None,
        metavar="DATE",
        help="YYYY-MM-DD or ISO 8601",
    )
    p.add_argument(
        "--end-published-date",
        default=None,
        metavar="DATE",
        help="YYYY-MM-DD or ISO 8601",
    )
    p.add_argument(
        "--moderation",
        default=None,
        metavar="LEVEL",
        help="moderation level",
    )
    p.add_argument(
        "--highlights", action="store_true", help="include key excerpts in results"
    )
    p.add_argument(
        "--summary", action="store_true", help="include LLM-generated summary"
    )
    p.add_argument(
        "--text", action="store_true", help="include full page text"
    )
    p.add_argument(
        "--max-characters",
        type=int,
        default=None,
        metavar="N",
        help="max characters per text result",
    )
    p.add_argument(
        "--max-age-hours",
        type=int,
        default=None,
        metavar="H",
        help="max age of cached content (0 = force livecrawl)",
    )
    p.add_argument(
        "--timeout", type=float, default=60.0, metavar="SEC", help="request timeout"
    )
    _add_output_args(p)
    p.set_defaults(func=cmd_search)

    # -- contents ----------------------------------------------------------
    p = sub.add_parser("contents", help="extract text/highlights from URLs")
    p.add_argument("urls", nargs="+", metavar="URL", help="URL(s) to extract")
    p.add_argument("--text", action="store_true", help="include full page text")
    p.add_argument(
        "--highlights", action="store_true", help="include key excerpts"
    )
    p.add_argument(
        "--summary", action="store_true", help="include LLM-generated summary"
    )
    p.add_argument(
        "--max-age-hours",
        type=int,
        default=None,
        metavar="H",
        help="max age of cached content",
    )
    p.add_argument(
        "--livecrawl-timeout",
        type=int,
        default=None,
        metavar="MS",
        help="livecrawl timeout in milliseconds",
    )
    p.add_argument(
        "--timeout", type=float, default=60.0, metavar="SEC", help="request timeout"
    )
    _add_output_args(p)
    p.set_defaults(func=cmd_contents)

    # -- context -----------------------------------------------------------
    p = sub.add_parser("context", help="get AI-generated context for a query")
    p.add_argument("query", help="natural language query")
    p.add_argument(
        "--tokens",
        default="dynamic",
        metavar="N",
        help='token budget: integer or "dynamic" (default: dynamic)',
    )
    p.add_argument(
        "--timeout", type=float, default=60.0, metavar="SEC", help="request timeout"
    )
    _add_output_args(p)
    p.set_defaults(func=cmd_context)

    # -- websets -----------------------------------------------------------
    ws = sub.add_parser("websets", help="manage Exa websets")
    ws_sub = ws.add_subparsers(dest="websets_cmd", required=True)

    # create
    wp = ws_sub.add_parser("create", help="create a webset")
    wp.add_argument("payload", nargs="?", default=None, help="JSON payload")
    wp.add_argument(
        "--file", type=argparse.FileType("r"), default=None, help="read JSON from file"
    )
    wp.add_argument(
        "--timeout", type=float, default=120.0, metavar="SEC", help="request timeout"
    )
    _add_output_args(wp)
    wp.set_defaults(func=cmd_websets_create)

    # get
    wp = ws_sub.add_parser("get", help="get a webset by ID")
    wp.add_argument("id", help="webset ID or externalId")
    wp.add_argument("--expand", default=None, metavar="FIELD", help="expand related objects (e.g. items)")
    wp.add_argument(
        "--timeout", type=float, default=60.0, metavar="SEC", help="request timeout"
    )
    _add_output_args(wp)
    wp.set_defaults(func=cmd_websets_get)

    # list
    wp = ws_sub.add_parser("list", help="list all websets")
    wp.add_argument("--cursor", default=None, help="pagination cursor")
    wp.add_argument("--limit", type=int, default=None, metavar="N", help="results per page")
    wp.add_argument(
        "--timeout", type=float, default=60.0, metavar="SEC", help="request timeout"
    )
    _add_output_args(wp)
    wp.set_defaults(func=cmd_websets_list)

    # items
    wp = ws_sub.add_parser("items", help="list items in a webset")
    wp.add_argument("webset_id", help="webset ID")
    wp.add_argument("--cursor", default=None, help="pagination cursor")
    wp.add_argument("--limit", type=int, default=None, metavar="N", help="results per page")
    wp.add_argument(
        "--timeout", type=float, default=60.0, metavar="SEC", help="request timeout"
    )
    _add_output_args(wp)
    wp.set_defaults(func=cmd_websets_items)

    # delete
    wp = ws_sub.add_parser("delete", help="delete a webset")
    wp.add_argument("id", help="webset ID or externalId")
    wp.add_argument("--force", action="store_true", help="confirm deletion")
    wp.add_argument(
        "--timeout", type=float, default=60.0, metavar="SEC", help="request timeout"
    )
    wp.add_argument(
        "--output",
        choices=["json", "compact"],
        default="json",
        help="output format (default: json)",
    )
    wp.set_defaults(func=cmd_websets_delete)

    # wait
    wp = ws_sub.add_parser("wait", help="poll until webset is idle")
    wp.add_argument("id", help="webset ID or externalId")
    wp.add_argument(
        "--timeout", type=float, default=300.0, metavar="SEC", help="max wait time"
    )
    wp.add_argument(
        "--interval", type=float, default=2.0, metavar="SEC", help="poll interval"
    )
    _add_output_args(wp)
    wp.set_defaults(func=cmd_websets_wait)

    # cancel
    wp = ws_sub.add_parser("cancel", help="cancel running webset operations")
    wp.add_argument("id", help="webset ID or externalId")
    wp.add_argument(
        "--timeout", type=float, default=60.0, metavar="SEC", help="request timeout"
    )
    wp.add_argument(
        "--output",
        choices=["json", "compact"],
        default="json",
        help="output format (default: json)",
    )
    wp.set_defaults(func=cmd_websets_cancel)

    return parser


####
## Entry point


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    try:
        rc = args.func(args)
    except KeyboardInterrupt:
        return 130
    return rc if isinstance(rc, int) else 0


if __name__ == "__main__":
    sys.exit(main())

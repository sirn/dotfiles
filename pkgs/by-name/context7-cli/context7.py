"""context7 — retrieve documentation context for libraries using the Context7 API."""

from __future__ import annotations

import argparse
import json
import os
import socket
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from typing import Any

_API_BASE = "https://context7.com/api/v2"


def _get_api_key() -> str:
    key = os.environ.get("CONTEXT7_API_KEY", "")
    if not key:
        print("context7: CONTEXT7_API_KEY is not set", file=sys.stderr)
        sys.exit(1)
    return key


def _request(
    path: str,
    *,
    params: dict[str, str] | None = None,
    timeout: float = 30.0,
) -> Any:
    """Send a GET request to the Context7 API, return parsed JSON or exit on error."""
    key = _get_api_key()

    qs = ""
    if params:
        qs = "?" + "&".join(f"{k}={urllib.parse.quote(v, safe='')}" for k, v in params.items())

    req = urllib.request.Request(
        f"{_API_BASE}{path}{qs}",
        headers={
            "Authorization": f"Bearer {key}",
        },
        method="GET",
    )

    max_retries = 3
    retry_count = 0

    while True:
        try:
            with urllib.request.urlopen(req, timeout=timeout) as resp:
                # Handle 301 redirect
                if resp.status == 301:
                    redirect_url = resp.headers.get("Location")
                    if redirect_url:
                        print(f"context7: following redirect to {redirect_url}", file=sys.stderr)
                        req = urllib.request.Request(
                            redirect_url,
                            headers={
                                "Authorization": f"Bearer {key}",
                            },
                            method="GET",
                        )
                        continue
                body = resp.read().decode("utf-8", errors="replace")
                content_type = resp.headers.get("Content-Type", "")
                if "application/json" in content_type:
                    return json.loads(body)
                return {"response": body}
        except urllib.error.HTTPError as exc:
            err_body = exc.read().decode("utf-8", errors="replace")

            # Handle 301 redirect
            if exc.code == 301:
                redirect_url = exc.headers.get("Location")
                if redirect_url:
                    print(f"context7: following redirect to {redirect_url}", file=sys.stderr)
                    req = urllib.request.Request(
                        redirect_url,
                        headers={
                            "Authorization": f"Bearer {key}",
                        },
                        method="GET",
                    )
                    continue

            # Handle 202 Accepted - retry with backoff
            if exc.code == 202:
                if retry_count < max_retries:
                    retry_count += 1
                    wait_time = 2 ** retry_count
                    print(f"context7: library not finalized, retrying in {wait_time}s...", file=sys.stderr)
                    time.sleep(wait_time)
                    continue
                print("context7: library not finalized after retries", file=sys.stderr)
                sys.exit(1)

            # Handle 429 Too Many Requests - respect Retry-After header
            if exc.code == 429:
                retry_after = exc.headers.get("Retry-After")
                if retry_after:
                    try:
                        wait_time = int(retry_after)
                    except ValueError:
                        wait_time = 60
                else:
                    wait_time = 60
                print(f"context7: rate limited, waiting {wait_time}s...", file=sys.stderr)
                time.sleep(wait_time)
                if retry_count < max_retries:
                    retry_count += 1
                    continue
                print("context7: rate limited after retries", file=sys.stderr)
                sys.exit(1)

            # Handle other error codes
            error_messages = {
                400: "Bad Request",
                401: "Unauthorized",
                403: "Forbidden",
                404: "Not Found",
                422: "Unprocessable Entity",
                500: "Internal Server Error",
                503: "Service Unavailable",
            }
            error_msg = error_messages.get(exc.code, f"HTTP {exc.code}")
            print(f"context7: {error_msg}: {err_body}", file=sys.stderr)
            sys.exit(1)

        except urllib.error.URLError as exc:
            if isinstance(exc.reason, socket.timeout):
                print("context7: request timed out", file=sys.stderr)
                sys.exit(124)
            print(f"context7: request failed: {exc.reason}", file=sys.stderr)
            sys.exit(1)

        # Max retries exceeded for 202
        if retry_count >= max_retries:
            break

    return None


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


def cmd_context(args: argparse.Namespace) -> int:
    tokens: Any = args.tokens
    if isinstance(tokens, str) and tokens != "dynamic":
        try:
            tokens = int(tokens)
        except ValueError:
            print('context7: --tokens must be an integer or "dynamic"', file=sys.stderr)
            return 2

    params: dict[str, str] = {
        "libraryId": args.libraryId,
        "query": args.query,
    }

    if tokens != "dynamic":
        params["tokensNum"] = str(tokens)

    if args.version:
        # Inject version into libraryId if provided
        params["libraryId"] = f"{args.libraryId}/{args.version}"

    data = _request("/context", params=params, timeout=args.timeout)

    if data is None:
        return 1

    if args.output == "text":
        # Text output: print raw response body (it's already markdown)
        response = data.get("response", data)
        if isinstance(response, str):
            print(response)
        else:
            _format_json(response, compact=(args.output == "compact"))
    else:
        # JSON/compact modes: wrap response in a JSON object
        wrapped = {
            "requestId": data.get("requestId", ""),
            "libraryId": args.libraryId,
            "query": args.query,
            "response": data.get("response", data),
        }
        _format_json(wrapped, compact=(args.output == "compact"))
    return 0


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="context7",
        description=__doc__,
    )

    sub = parser.add_subparsers(dest="cmd")

    # context subcommand (the only command)
    p = sub.add_parser("context", help="get documentation context for a library")
    p.add_argument("libraryId", help="library identifier (e.g., /facebook/react)")
    p.add_argument("-q", "--query", required=True, help="natural language query")
    p.add_argument(
        "-t", "--tokens",
        default="dynamic",
        metavar="N",
        help='token budget: integer or "dynamic" (default: dynamic)',
    )
    p.add_argument(
        "-v", "--version",
        default=None,
        metavar="VERSION",
        help="pin to exact version (e.g., v15.1.8)",
    )
    p.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        metavar="SEC",
        help="request timeout in seconds (default: 30)",
    )
    _add_output_args(p)
    p.set_defaults(func=cmd_context)

    return parser


def main(argv: list[str] | None = None) -> int:
    if argv is None:
        argv = sys.argv[1:]

    # If first arg looks like a libraryId (starts with /), prepend 'context' subcommand
    if argv and argv[0].startswith("/"):
        argv = ["context"] + argv

    args = build_parser().parse_args(argv)

    if args.cmd is None:
        build_parser().print_help()
        return 1

    try:
        rc = args.func(args)
    except KeyboardInterrupt:
        return 130
    return rc if isinstance(rc, int) else 0

if __name__ == "__main__":
    sys.exit(main())

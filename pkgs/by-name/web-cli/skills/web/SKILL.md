---
name: web
description: Inspect web pages with headless Chromium - one-shot URL fetch/text/screenshot/eval, or long-lived sessions for click/fill/navigate flows. Use when post-JS DOM, dynamic content, multi-step interaction, or visual rendering is required.
---

Use `web` for browser-rendered web inspection. Reach for it only when JavaScript execution, dynamic DOM, multi-step interaction, or visual rendering is needed; otherwise prefer `curl` (and `jaq` for JSON APIs), which is faster and cheaper.

## Decision Rules

- Static HTML or JSON API? Use `curl`, optionally piped through `jaq`.
- Single-page render of an SPA? Use **one-shot** mode (give a URL).
- Need to click, fill forms, navigate multiple pages, or stay logged in? Use **session** mode (`--session NAME`).
- Login or session flows? Use `web session save-state` after authenticating, then re-use with `--state-file` on the next `session start`.
- Untrusted URLs from user-controlled input? Confirm intent before fetching.

## Bimodal commands

Each accepts either a positional URL (one-shot, fresh browser per call) or `--session NAME` (attached to a running session). The two are mutually exclusive.

| Command                                           | One-shot                     | Session (`--session NAME`)                       |
| ------------------------------------------------- | ---------------------------- | ------------------------------------------------ |
| `web html [URL\|SELECTOR]`                        | rendered HTML of URL         | `outerHTML` of selector, or full page if omitted |
| `web text [URL\|SELECTOR]`                        | body innerText of URL        | innerText of selector (default `body`)           |
| `web eval URL EXPR` / `web eval EXPR --session N` | run JS on freshly loaded URL | run JS in current page                           |
| `web screenshot [URL] -o FILE`                    | PNG of URL                   | PNG of current page                              |
| `web pdf [URL] -o FILE`                           | PDF of URL                   | PDF of current page                              |
| `web links [URL]`                                 | anchors from URL             | anchors from current page                        |

Output to file path or `-` for stdout. PNG/PDF binary; everything else JSON or plain text.

One-shot-only flags: `--wait-for SELECTOR`, `--wait-until {domcontentloaded,load,networkidle,commit}`, `--viewport WxH`, `--user-agent`, `--header K:V` (repeatable). All commands accept `--timeout MS` (default 30000).

## One-shot only

- `web console <url>` - capture `console.*` and `pageerror` during load (JSONL).
- `web network <url>` - log responses (JSONL: `{url, method, status, mimeType, sizeBytes, resourceType}`).

## Session mode

Long-lived browser. Keeps page state (cookies, localStorage, scroll position, current URL) across commands. Use absolute paths for output files; the daemon's CWD is not yours.

### Lifecycle

- `web session start [--name N] [--state-file F]` - launch daemon. Default name: `default`.
- `web session list` - active sessions (JSONL).
- `web session stop [--name N]` - terminate daemon and clean up.
- `web session save-state --session N -o FILE` - dump cookies/localStorage for re-use.

Always stop sessions you start; they hold a chromium process until killed.

### Page actions (session-only; require running session)

All require `--session NAME` and accept `--timeout MS`. There is no implicit default session - even when only one session is running, you must name it.

- `web nav <url> [--wait-until ...] [--wait-for SELECTOR]`
- `web click <selector> [--no-wait-after] [--force]`
- `web fill <selector> <value> [--no-wait-after]`
- `web press <selector> <key> [--no-wait-after]`
- `web wait <selector> [--state visible|hidden|attached|detached]`
- `web back` / `web forward` / `web reload`
- `web url` - current URL and title (JSON).

`--no-wait-after` (on `click`/`fill`/`press`): don't wait for any post-action navigation to start. Use when an action triggers a slow/hanging navigation and you only want the action dispatched. `--force` (on `click`): skip actionability checks (visible/stable/etc.); useful when an overlay you can't dismiss is intercepting clicks.

## Output

- `html`, `text` - plain text/HTML to stdout.
- `screenshot`, `pdf` - binary; `-o FILE` to file, `-o -` to stdout.
- `eval`, `links`, `nav`, `url` - single JSON value.
- `console`, `network`, `session list` - JSONL; pipe to `jaq` for filtering.

## Examples

```sh
# One-shot: rendered DOM of an SPA
web html https://example.com

# One-shot: capture console errors during load
web console https://example.com | jaq 'select(.type == "error" or .type == "pageerror")'

# Interactive flow: search and read results
web session start
web nav --session default https://duckduckgo.com
web fill --session default 'input[name=q]' 'site:example.com'
web press --session default 'input[name=q]' Enter
web wait --session default 'a[data-testid="result-title-a"]'
web text --session default '#react-layout main' | head -100
web session stop

# Persist auth across runs
web session start --name work
web nav --session work https://app.example.com/login
web fill --session work '#email' 'me@example.com'
web fill --session work '#password' "$PASSWORD"
web click --session work 'button[type=submit]'
web wait --session work '.dashboard'
web session save-state --session work -o /tmp/work-state.json
web session stop --name work

# Reuse the saved auth on later runs
web session start --name work --state-file /tmp/work-state.json
web nav --session work https://app.example.com/admin
web text --session work '.admin-content'
web session stop --name work
```

## Screenshots for inspection

When taking a screenshot only to look at the page (not to keep), create a temp directory, save there, read it back, then remove the directory. Read the PNG file directly (do not OCR or pipe to `file`); your vision capability handles it. Pattern:

```sh
shotdir=$(mktemp -d)
trap 'rm -rf "$shotdir"' EXIT
web screenshot https://example.com -o "$shotdir/page.png" --full-page
# Read "$shotdir/page.png" with the Read tool to inspect visually.
```

Use a deliberate path under the project (e.g. `tmp/`) only when the user asks to keep the screenshot.

## Diagnosing hangs and timeouts

`click` (and `fill`/`press`) wait for the element to be **attached, visible, stable, enabled, and receive pointer events**, then for any navigation the action triggers to start loading. Common causes when these hang or time out:

- **Selector matches zero or many** elements. Verify with `web eval --session N 'document.querySelectorAll("YOUR_SEL").length'`.
- **Element is hidden / off-screen / mid-animation**. Verify with `web eval --session N '!!document.querySelector("YOUR_SEL")?.offsetParent'`.
- **Cookie banner or modal intercepts clicks**. Click the banner away first, or pass `--force` to skip actionability checks.
- **Click triggers a hanging navigation** (slow TLS, infinite redirect). Pass `--no-wait-after` to dispatch the click without waiting for the resulting nav.
- **A previous op is stuck** (e.g. `nav --wait-until networkidle` on a long-poll page). All later ops queue behind it. Run `web session list` - if it doesn't return promptly, the daemon is stuck. Run `web session stop` and start fresh.

A daemon-side wall-clock guard kills any op that exceeds `--timeout + 5s` so a stuck op can't pin the session indefinitely; the failed op surfaces a `TimeoutError` and the next op proceeds.

## Notes

- Only chromium is bundled; firefox and webkit are not available.
- `web` launches a fresh browser per one-shot invocation. For more than two requests against the same site, use a session.
- Session daemons are per-user and isolated under `$XDG_RUNTIME_DIR/web-cli/`. Different `--name` values are independent sessions; the same name is one shared session.
- One operation at a time per session (the daemon serializes requests).
- Screenshots and PDFs are written to a path you choose; do not dump them into the repository.

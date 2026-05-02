---
name: web
description: Inspect web pages with headless Chromium - one-shot URL fetch/text/screenshot/eval, or long-lived sessions for click/fill/navigate flows. Use when post-JS DOM, dynamic content, multi-step interaction, or visual rendering is required.
---

Use `web` for browser-rendered web inspection. Prefer `curl` for static HTML and JSON APIs. Use `web` when JavaScript, dynamic DOM, clicking, forms, session state, or visual screenshots matter.

## Default agent workflow: Bash -> Read -> Bash

Use this pattern for visual browsing. The first Bash block prints the exact PNG path to inspect. Then use the agent Read tool on that path. Only after Read succeeds, run the cleanup block.

### 1. Bash: start browser, navigate, screenshot

```sh
web session start --name browse
web nav --session browse https://example.com

state=/tmp/web-cli-browse.dir
shotdir=$(mktemp -d)
printf '%s\n' "$shotdir" > "$state"

shot="$shotdir/page.png"
web screenshot -o "$shot" --session browse --full-page
printf 'READ_THIS_SCREENSHOT=%s\n' "$shot"
```

### 2. Agent tool call: Read the PNG

Call the agent Read tool on the path printed after `READ_THIS_SCREENSHOT=`. Do not type `Read` into the shell.

### 3. Bash: clean up when finished

```sh
state=/tmp/web-cli-browse.dir
shotdir=$(cat "$state")
rm -rf "$shotdir" "$state"
web session stop --name browse
```

## Five rules agents must follow

1. Every session command needs `--session NAME`. There is no implicit default, even if only one session is running.
2. Session screenshots need absolute paths. `shotdir=$(mktemp -d)` gives an absolute temp directory.
3. After `web screenshot`, call the agent Read tool on the PNG path. The CLI does not show the image inline.
4. For external links, prefer `web eval` to extract `href`, strip JSON quotes, then `web nav`. `web click` can hang on slow or bot-protected navigation.
5. Always clean up temp dirs and stop sessions you start.

## Decide what to use

- Static HTML or JSON API? Use `curl`, optionally piped through `jaq`.
- One rendered page, no interaction? Use one-shot commands: `web html URL`, `web text URL`, or `web screenshot URL -o FILE`.
- Click, fill, back, multiple pages, login, or repeated screenshots? Use a named session.
- Need to see the page? Screenshot to a temp path, then use the agent Read tool.
- Need persistent auth? Log in in a session, `web session save-state --session NAME -o FILE`, then reuse with `web session start --name NAME --state-file FILE`.
- User-controlled or suspicious URL? Confirm before fetching.

## Recipes

### One rendered page, no interaction

```sh
web html https://example.com
web text https://example.com
```

### One-shot screenshot, then Read

Use this for one visual check without interaction.

```sh
state=/tmp/web-cli-oneshot.dir
shotdir=$(mktemp -d)
printf '%s\n' "$shotdir" > "$state"

shot="$shotdir/page.png"
web screenshot https://example.com -o "$shot" --full-page
printf 'READ_THIS_SCREENSHOT=%s\n' "$shot"
```

Agent step: Read the printed PNG path. Then clean up:

```sh
state=/tmp/web-cli-oneshot.dir
shotdir=$(cat "$state")
rm -rf "$shotdir" "$state"
```

### Session visual loop: see, decide, act, see again

Use this when you need to inspect a page visually, choose the next action, then inspect the next page.

First page:

```sh
web session start --name browse
web nav --session browse https://en.wikipedia.org/wiki/Main_Page

state=/tmp/web-cli-browse.dir
shotdir=$(mktemp -d)
printf '%s\n' "$shotdir" > "$state"

shot="$shotdir/page1.png"
web screenshot -o "$shot" --session browse --full-page
printf 'READ_THIS_SCREENSHOT=%s\n' "$shot"
```

Agent step: Read the printed PNG path. Then continue with the same session. Example next page:

```sh
state=/tmp/web-cli-browse.dir
shotdir=$(cat "$state")

web nav --session browse 'https://en.wikipedia.org/wiki/Chromium_(web_browser)'

shot="$shotdir/page2.png"
web screenshot -o "$shot" --session browse --full-page
printf 'READ_THIS_SCREENSHOT=%s\n' "$shot"
```

Agent step: Read the new printed PNG path. Repeat navigate/click/fill/screenshot as needed. When done:

```sh
state=/tmp/web-cli-browse.dir
shotdir=$(cat "$state")
rm -rf "$shotdir" "$state"
web session stop --name browse
```

### Fill a form, submit, then inspect result

Use stable selectors. Do not assume default HTML behavior appears as attributes; a form button may submit even when the HTML has no `type="submit"`.

```sh
web session start --name formtest
web nav --session formtest https://httpbin.org/forms/post

web fill --session formtest 'input[name=custname]' 'Alice'
web fill --session formtest 'textarea[name=comments]' 'Hello world'

# Use --no-wait-after when the submit navigation may be slow.
web click --session formtest 'button' --no-wait-after
web wait --session formtest 'pre'

state=/tmp/web-cli-formtest.dir
shotdir=$(mktemp -d)
printf '%s\n' "$shotdir" > "$state"

shot="$shotdir/result.png"
web screenshot -o "$shot" --session formtest --full-page
printf 'READ_THIS_SCREENSHOT=%s\n' "$shot"
```

Agent step: Read the printed PNG path. Then clean up:

```sh
state=/tmp/web-cli-formtest.dir
shotdir=$(cat "$state")
rm -rf "$shotdir" "$state"
web session stop --name formtest
```

### Follow a link reliably

For external links, prefer `eval href -> strip JSON quotes -> nav`. This avoids `web click` hanging while it waits for a slow external navigation.

```sh
web session start --name browse
web nav --session browse https://example.com

# web eval returns JSON. A string result includes literal double quotes.
link=$(web eval --session browse 'document.querySelector("a").href' | tr -d '"')
web nav --session browse "$link"
web wait --session browse 'body'

web url --session browse
web session stop --name browse
```

### Template: login flow and saved auth

This is a template, not a directly runnable example. Replace URLs and selectors with the real app's values. Do not print or hardcode secrets.

```sh
web session start --name work
web nav --session work https://app.example.com/login
web fill --session work '#email' 'me@example.com'
web fill --session work '#password' "$PASSWORD"
web click --session work 'button[type=submit]'
web wait --session work '.dashboard'
web session save-state --session work -o /tmp/work-state.json
web session stop --name work
```

Reuse later:

```sh
web session start --name work --state-file /tmp/work-state.json
web nav --session work https://app.example.com/admin
web text --session work '.admin-content'
web session stop --name work
```

## Bad / good patterns

### Session screenshots

```sh
# Bad: relative path is resolved by the browser daemon, not your shell CWD.
web screenshot -o page.png --session browse

# Good: absolute temp path.
shotdir=$(mktemp -d)
web screenshot -o "$shotdir/page.png" --session browse
```

### Screenshot inspection

```sh
# Bad: screenshot created, but never inspected.
web screenshot -o "$shotdir/page.png" --session browse

# Good: print the path, then call the agent Read tool on that PNG.
web screenshot -o "$shotdir/page.png" --session browse
printf 'READ_THIS_SCREENSHOT=%s\n' "$shotdir/page.png"
```

### URLs from `web eval`

```sh
# Bad: $link includes JSON quote characters, like "https://example.com".
link=$(web eval --session browse 'document.querySelector("a").href')
web nav --session browse "$link"

# Good: strip the JSON quotes first.
link=$(web eval --session browse 'document.querySelector("a").href' | tr -d '"')
web nav --session browse "$link"
```

### Waiting for pages

```sh
# Bad: networkidle may never resolve on long-polling or websocket pages.
web nav --session browse https://example.com --wait-until networkidle

# Good: navigate normally, then wait for a readiness selector.
web nav --session browse https://example.com
web wait --session browse 'body'
```

## Command reference

### Bimodal commands

Each accepts either a positional URL for one-shot mode or `--session NAME` for a running session. Do not use both in the same command.

| Command                                           | One-shot                     | Session (`--session NAME`)                       |
| ------------------------------------------------- | ---------------------------- | ------------------------------------------------ |
| `web html [URL\|SELECTOR]`                        | rendered HTML of URL         | `outerHTML` of selector, or full page if omitted |
| `web text [URL\|SELECTOR]`                        | body innerText of URL        | innerText of selector, default `body`            |
| `web eval URL EXPR` / `web eval EXPR --session N` | run JS on freshly loaded URL | run JS in current page                           |
| `web screenshot [URL] -o FILE`                    | PNG of URL                   | PNG of current page                              |
| `web pdf [URL] -o FILE`                           | PDF of URL                   | PDF of current page                              |
| `web links [URL]`                                 | anchors from URL             | anchors from current page                        |

Output to file path or `-` for stdout. PNG/PDF are binary; everything else is JSON or plain text.

One-shot-only flags: `--wait-for SELECTOR`, `--wait-until {domcontentloaded,load,networkidle,commit}`, `--viewport WxH`, `--user-agent`, `--header K:V` (repeatable). All commands accept `--timeout MS` (default 30000).

### One-shot-only commands

- `web console URL` - capture `console.*` and `pageerror` during load, as JSONL.
- `web network URL` - log responses as JSONL: `{url, method, status, mimeType, sizeBytes, resourceType}`.

### Session lifecycle

A long-lived browser session keeps cookies, localStorage, scroll position, and current URL across commands. Use absolute paths for output files because the browser daemon's CWD is not your shell CWD.

- `web session start --name NAME [--state-file F]` - launch daemon.
- `web session list` - active sessions, as JSONL.
- `web session stop --name NAME` - terminate daemon and clean up.
- `web session save-state --session NAME -o FILE` - dump cookies/localStorage for reuse.

### Session-only page actions

All require `--session NAME` and accept `--timeout MS`.

- `web nav URL [--wait-until ...] [--wait-for SELECTOR]`
- `web click SELECTOR [--no-wait-after] [--force]`
- `web fill SELECTOR VALUE [--no-wait-after]`
- `web press SELECTOR KEY [--no-wait-after]`
- `web wait SELECTOR [--state visible|hidden|attached|detached]`
- `web back` / `web forward` / `web reload`
- `web url` - current URL and title, as JSON.

`--no-wait-after`: for `click`, `fill`, and `press`; dispatch the action without waiting for any navigation it triggers. Use it when navigation is slow or hanging.

`--force`: for `click`; skip actionability checks. Use it when an overlay or animation prevents a normal click and you intentionally want to click anyway.

### Output formats

- `html`, `text` - plain text/HTML to stdout.
- `screenshot`, `pdf` - binary; use `-o FILE` or `-o -`.
- `eval`, `links`, `nav`, `url` - single JSON value.
- `console`, `network`, `session list` - JSONL; pipe to `jaq` for filtering.

## Diagnosing hangs and timeouts

`click`, `fill`, and `press` wait for the element to be attached, visible, stable, enabled, receiving pointer events, and then wait for triggered navigation to start. Common causes of hangs:

- Selector matches zero or many elements. Check with `web eval --session NAME 'document.querySelectorAll("YOUR_SEL").length'`.
- Element is hidden, off-screen, or animating. Check with `web eval --session NAME '!!document.querySelector("YOUR_SEL")?.offsetParent'`.
- Cookie banner or modal intercepts clicks. Dismiss it first, or use `--force` if intentional.
- Click triggers slow/hanging navigation. Use `--no-wait-after`, then wait for a selector yourself.
- Previous operation is stuck. Run `web session list`; if it hangs, stop the session and start fresh.

A daemon-side wall-clock guard kills any operation that exceeds `--timeout + 5s`, so a stuck operation should eventually return a `TimeoutError`.

## Common mistakes to avoid

- Forgetting `--session NAME` on every session command.
- Running cleanup before using the Read tool on the screenshot.
- Using a relative `-o` path in session mode.
- Passing both a URL and `--session` to a bimodal command.
- Passing raw `web eval` string output to `web nav` without stripping JSON quotes.
- Saving screenshots or PDFs in the repository unless the user asks to keep them.

## Notes

- Only Chromium is bundled; Firefox and WebKit are not available.
- `web` launches a fresh browser for each one-shot invocation. For more than two requests against the same site, use a session.
- Session daemons are per-user and isolated under `$XDG_RUNTIME_DIR/web-cli/`. Different `--name` values are independent sessions; the same name is one shared session.
- One operation runs at a time per session; the daemon serializes requests.
- Screenshots and PDFs are written to a path you choose; prefer temp dirs.

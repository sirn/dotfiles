---
name: synthetic-search
description: Search the web using the Synthetic Search API with zero-data-retention. Use for privacy-sensitive queries, general web research, finding documentation, and current information.
---

Web search using the Synthetic Search API.

## Prerequisites

- `SYNTHETIC_API_KEY` environment variable with a valid Synthetic Search API key

**Check before use:**

```bash
[ -z "$SYNTHETIC_API_KEY" ] && echo "Error: SYNTHETIC_API_KEY not set" || echo "OK: SYNTHETIC_API_KEY is set"
```

**Note:** Never hardcode `SYNTHETIC_API_KEY`. Always use the environment variable.

## Output Modes

| Flag | Output format | Use case |
| ---- | ------------- | -------- |
| *(default)* | JSON (pretty) | Programmatic use, saving to file |
| `--compact` | JSON (no whitespace) | Piping to `jaq` |
| `--list` | `Title: URL` per line | Quick scanning of result URLs |
| `--text` | Numbered with title, URL, snippet | Human-readable review of results |

Flags `--compact`, `--list`, and `--text` are mutually exclusive.

```bash
# JSON output (default)
synthetic-search "rust async trait methods"

# Compact JSON for piping
synthetic-search --compact "rust async trait methods"

# Title + URL per line
synthetic-search --list "rust async trait methods"

# Human-readable with snippets
synthetic-search --text "rust async trait methods"

# Custom timeout (any mode)
synthetic-search --text --timeout 60 "obscure technical topic"
```

## Response (JSON modes)

```json
{
  "results": [
    {
      "url": "https://example.com/page",
      "title": "Page Title",
      "text": "Snippet or excerpt describing the page content...",
      "published": "2025-01-15"
    }
  ]
}
```

## Example Usage

```bash
# Quick scan of result URLs
synthetic-search --list "python asyncio patterns"

# Save JSON results to a file
synthetic-search "nix flake development shell" > nix-results.json

# Readable review with context
synthetic-search --text "svelte 5 runes $state"
```

## Response Fields

| Field                 | Type   | Description                                  |
| --------------------- | ------ | -------------------------------------------- |
| `results`             | array  | List of search results                       |
| `results[].url`       | string | Page URL                                     |
| `results[].title`     | string | Page title                                   |
| `results[].text`      | string | Text snippet or excerpt from the page        |
| `results[].published` | string | Publication date (ISO 8601 format, if known) |

## Query Constraints

- Natural language queries work best. Describe what you're looking for in plain English.
- No advanced search operators (no `site:`, `filetype:`, Boolean, or filter parameters).
- No pagination or result count control. The API returns the most relevant results for your query.
- Zero data retention: queries are not stored by the provider.

## Error Handling

| Status | Meaning               | Action                                      |
| ------ | --------------------- | ------------------------------------------- |
| `200`  | Success               | Process results normally                    |
| `400`  | Bad Request           | Check query content                         |
| `401`  | Unauthorized          | Verify `SYNTHETIC_API_KEY` is set and valid |
| `429`  | Rate limit exceeded   | Wait and retry with exponential backoff     |
| `500`  | Internal Server Error | Retry later                                 |

Exit codes: `0` = success, `1` = error, `124` = timeout, `130` = interrupt.

## Best Practices

1. **Use natural language**: Write queries as plain English descriptions of what you're looking for.

2. **Choose the right mode**: `--list` for quick URL scanning, `--text` for reading snippets, `--compact | jaq` for custom filtering.

3. **Cache responses**: Search results for stable topics can be reused for the duration of a session.

4. **Handle errors gracefully**: Check the exit code and implement retry logic for transient failures.

5. **Respect zero data retention**: This API is designed for privacy-sensitive queries where data retention matters.

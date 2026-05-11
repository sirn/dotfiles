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

## Web Search

Perform a web search query. The API accepts natural language queries and returns results with URLs, titles, text snippets, and publication dates.

```bash
# Basic search (pretty-printed JSON)
synthetic-search "your search query here"

# Compact output (for piping to jaq)
synthetic-search --compact "rust async trait methods"

# Custom timeout (seconds)
synthetic-search --timeout 60 "svelte 5 runes $state"

# Raw API response bytes
synthetic-search --raw "python asyncio patterns"

# Explicit subcommand form (equivalent)
synthetic-search search "nix flake development shell"
```

## Response

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
# Extract URLs and titles with jaq
synthetic-search --compact "python asyncio patterns" | \
  jaq -r '.results[] | "\(.title): \(.url)"'

# Save results to a file
synthetic-search "nix flake development shell" > nix-results.json

# Search with longer timeout for slow responses
synthetic-search --timeout 60 "obscure technical topic"
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

2. **Extract with jaq**: Use `--compact` and pipe to `jaq` for filtering relevant fields.

3. **Cache responses**: Search results for stable topics can be reused for the duration of a session.

4. **Handle errors gracefully**: Check the exit code and implement retry logic for transient failures.

5. **Respect zero data retention**: This API is designed for privacy-sensitive queries where data retention matters.

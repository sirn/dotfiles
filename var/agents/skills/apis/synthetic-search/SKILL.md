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

## API Endpoint

```
POST https://api.synthetic.new/v2/search
```

## Web Search

Perform a web search query. The API accepts natural language queries and returns results with URLs, titles, text snippets, and publication dates.

**Request**:

```bash
curl -s -X POST "https://api.synthetic.new/v2/search" \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"query": "your search query here"}'
```

**Response**:

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
# Basic search
curl -s -X POST "https://api.synthetic.new/v2/search" \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"query": "rust async trait methods"}' | jq .

# Search for current documentation
curl -s -X POST "https://api.synthetic.new/v2/search" \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"query": "svelte 5 runes $state"}' | jq .

# Extract just URLs and titles
curl -s -X POST "https://api.synthetic.new/v2/search" \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"query": "python asyncio patterns"}' | \
  jq -r '.results[] | "\(.title): \(.url)"'

# Save results to a file
curl -s -X POST "https://api.synthetic.new/v2/search" \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"query": "nix flake development shell"}' > nix-results.json
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
| `400`  | Bad Request           | Check JSON body format and query content    |
| `401`  | Unauthorized          | Verify `SYNTHETIC_API_KEY` is set and valid |
| `429`  | Rate limit exceeded   | Wait and retry with exponential backoff     |
| `500`  | Internal Server Error | Retry later                                 |

## Best Practices

1. **Check the key first**: Always verify `SYNTHETIC_API_KEY` is set before making requests.

2. **Use natural language**: Write queries as plain English descriptions of what you're looking for.

3. **Extract with jq**: Use `jq` to filter relevant fields from results.

4. **Cache responses**: Search results for stable topics can be reused for the duration of a session.

5. **Handle errors gracefully**: Check the HTTP status code and implement retry logic for `429` and `500` responses.

6. **Respect zero data retention**: This API is designed for privacy-sensitive queries where data retention matters.

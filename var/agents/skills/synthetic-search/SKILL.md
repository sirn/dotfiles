---
name: synthetic-search
description: Search the web using Synthetic's zero-data-retention web search API.
---

Web search using the Synthetic Search API.

## Prerequisites

Requires `SYNTHETIC_API_KEY` environment variable to be set.

## API Endpoint

```
POST https://api.synthetic.new/v2/search
```

## Search

Perform a web search query.

**Request**:
```bash
curl -s https://api.synthetic.new/v2/search \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"query": "your search query"}'
```

**Response**:
```json
{
  "results": [
    {
      "url": "https://example.com/page",
      "title": "Page Title",
      "text": "Snippet or summary of the page content...",
      "published": "2025-11-05T00:00:00.000Z"
    }
  ]
}
```

## Example Usage

```bash
# Search for Python requests documentation
curl -s https://api.synthetic.new/v2/search \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"query": "python requests library documentation"}' | jq .

# Search with jq to extract just URLs
curl -s https://api.synthetic.new/v2/search \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"query": "nix flake tutorial"}' | jq -r '.results[].url'

# Search and get titles with URLs
curl -s https://api.synthetic.new/v2/search \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY" \
  -H "Content-Type: application/json" \
  -d '{"query": "rust async await"}' | jq -r '.results[] | "\(.title): \(.url)"'
```

## Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `results` | array | List of search results |
| `results[].url` | string | URL of the result |
| `results[].title` | string | Title of the page |
| `results[].text` | string | Snippet/summary of the content |
| `results[].published` | string | ISO 8601 publication date |

## Error Handling

The API will return appropriate HTTP status codes:
- `200` - Success
- `401` - Invalid or missing API key
- `429` - Rate limit exceeded
- `500` - Server error

Always check that `SYNTHETIC_API_KEY` is set before making requests.

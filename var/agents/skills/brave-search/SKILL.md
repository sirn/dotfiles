---
name: brave-search
description: Search the web using Brave Search API with web, news, image, and video search capabilities.
---

Web search using the Brave Search API.

## Prerequisites

Requires `BRAVE_API_KEY` environment variable to be set.

## API Endpoint

```
GET https://api.search.brave.com/res/v1/web/search
```

## Web Search

Perform a web search query.

**Request**:
```bash
curl -s "https://api.search.brave.com/res/v1/web/search?q=your+query" \
  -H "X-Subscription-Token: $BRAVE_API_KEY" \
  -H "Accept: application/json"
```

**Response**:
```json
{
  "query": {
    "original": "your query",
    "more_results_available": true
  },
  "web": {
    "results": [
      {
        "title": "Result Title",
        "url": "https://example.com/page",
        "description": "Snippet describing the page content...",
        "extra_snippets": ["Additional excerpt 1", "Additional excerpt 2"],
        "page_age": "2025-01-15",
        "profile": {
          "name": "Site Name",
          "img": "https://example.com/favicon.ico"
        }
      }
    ]
  }
}
```

## Example Usage

```bash
# Basic search
curl -s "https://api.search.brave.com/res/v1/web/search?q=rust+web+frameworks" \
  -H "X-Subscription-Token: $BRAVE_API_KEY" | jq .

# Search with freshness filter (pd=day, pw=week, pm=month, py=year)
curl -s "https://api.search.brave.com/res/v1/web/search?q=ai+news&freshness=pw" \
  -H "X-Subscription-Token: $BRAVE_API_KEY" | jq .

# Search with country and language targeting
curl -s "https://api.search.brave.com/res/v1/web/search?q=nachhaltige+energie&country=DE&search_lang=de" \
  -H "X-Subscription-Token: $BRAVE_API_KEY" | jq .

# Extract just URLs and titles
curl -s "https://api.search.brave.com/res/v1/web/search?q=python+tutorials" \
  -H "X-Subscription-Token: $BRAVE_API_KEY" | \
  jq -r '.web.results[] | "\(.title): \(.url)"'
```

## Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `q` | string | **Required.** Search query (URL-encoded). |
| `count` | integer | Results per page (max 20, default 20). |
| `offset` | integer | Pagination offset (0-9, default 0). |
| `freshness` | string | Time filter: `pd` (24h), `pw` (week), `pm` (month), `py` (year), or date range `YYYY-MM-DDtoYYYY-MM-DD`. |
| `country` | string | 2-letter country code (e.g., `US`, `DE`, `JP`). |
| `search_lang` | string | Content language (ISO 639-1, e.g., `en`, `de`). |
| `ui_lang` | string | UI language (e.g., `en-US`). |
| `safesearch` | string | Filter level: `off`, `moderate` (default), `strict`. |
| `extra_snippets` | boolean | Include additional excerpts per result. |
| `enable_rich_callback` | integer | Set to `1` for rich results (weather, stocks, etc.). |

## Search Operators

Include operators directly in the `q` parameter:

| Operator | Example | Description |
|----------|---------|-------------|
| `""` | `"climate change"` | Exact phrase match |
| `-` | `javascript -jquery` | Exclude term |
| `site:` | `site:github.com rust` | Search specific site |
| `filetype:` | `filetype:pdf machine learning` | Filter by file type |

## Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `query.original` | string | The search query |
| `query.more_results_available` | boolean | Whether more results exist |
| `web.results[]` | array | Search results |
| `web.results[].title` | string | Page title |
| `web.results[].url` | string | Page URL |
| `web.results[].description` | string | Main snippet |
| `web.results[].extra_snippets[]` | array | Additional excerpts (if requested) |
| `web.results[].page_age` | string | Page publication date |
| `web.results[].profile.name` | string | Site name |

## Pagination

Check `query.more_results_available` before requesting the next page:

```bash
# Page 1 (offset=0, default)
curl -s "https://api.search.brave.com/res/v1/web/search?q=open+source&count=20" \
  -H "X-Subscription-Token: $BRAVE_API_KEY" | jq '.query.more_results_available'

# Page 2 (offset=1)
curl -s "https://api.search.brave.com/res/v1/web/search?q=open+source&count=20&offset=1" \
  -H "X-Subscription-Token: $BRAVE_API_KEY" | jq .
```

## Error Handling

| Status | Meaning |
|--------|---------|
| `200` | Success |
| `401` | Invalid or missing API key |
| `429` | Rate limit exceeded |
| `500` | Server error |

## Additional Search Endpoints

### News Search
```bash
curl -s "https://api.search.brave.com/res/v1/news/search?q=breaking+news" \
  -H "X-Subscription-Token: $BRAVE_API_KEY"
```

### Image Search
```bash
curl -s "https://api.search.brave.com/res/v1/images/search?q=nature+landscape" \
  -H "X-Subscription-Token: $BRAVE_API_KEY"
```

### Video Search
```bash
curl -s "https://api.search.brave.com/res/v1/videos/search?q=tutorials" \
  -H "X-Subscription-Token: $BRAVE_API_KEY"
```

Always check that `BRAVE_API_KEY` is set before making requests.

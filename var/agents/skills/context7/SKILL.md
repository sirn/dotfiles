---
name: context7
description: Retrieve up-to-date documentation context for libraries using the Context7 API.
---

Documentation context retrieval using the Context7 API.

## Prerequisites

Requires `CONTEXT7_API_KEY` environment variable to be set. API keys start with `ctx7sk`.

## API Endpoints

| Endpoint | Description |
|----------|-------------|
| `GET /api/v2/libraries` | Search for indexed libraries |
| `GET /api/v2/context` | Retrieve documentation context for a library |

## Search Libraries

Find indexed libraries before requesting context.

**Request**:
```bash
curl -s "https://context7.com/api/v2/libraries?q=next.js" \
  -H "Authorization: Bearer $CONTEXT7_API_KEY"
```

**Response**:
```json
[
  {
    "id": "/vercel/next.js",
    "name": "next",
    "description": "The React Framework for the Web",
    "links": {
      "github": "https://github.com/vercel/next.js"
    },
    "language": "typescript",
    "stars": 123456,
    "lastUpdated": "2025-01-15T00:00:00.000Z",
    "versions": ["v15.1.8", "v14.2.20"]
  }
]
```

## Get Context

Retrieve documentation context for a specific library.

**Request**:
```bash
curl -s "https://context7.com/api/v2/context?libraryId=/vercel/next.js&query=How%20to%20use%20useState" \
  -H "Authorization: Bearer $CONTEXT7_API_KEY"
```

**Response**:
```json
{
  "id": "doc-123",
  "title": "useState - React",
  "content": "const [state, setState] = useState(initialState); ...",
  "metadata": {
    "source": "https://react.dev/reference/react/useState",
    "library": "/vercel/next.js",
    "version": "v15.1.8"
  }
}
```

## Example Usage

```bash
# Step 1: Find a library
LIBRARY=$(curl -s "https://context7.com/api/v2/libraries?q=react" \
  -H "Authorization: Bearer $CONTEXT7_API_KEY" | \
  jq -r '.[0].id')

# Step 2: Get context for a specific topic
curl -s "https://context7.com/api/v2/context?libraryId=${LIBRARY}&query=hooks" \
  -H "Authorization: Bearer $CONTEXT7_API_KEY" | jq .

# Search with specific version
curl -s "https://context7.com/api/v2/context?libraryId=/vercel/next.js/v15.1.8&query=app%20router" \
  -H "Authorization: Bearer $CONTEXT7_API_KEY" | jq .

# Extract just the content
curl -s "https://context7.com/api/v2/context?libraryId=/vercel/next.js&query=useState" \
  -H "Authorization: Bearer $CONTEXT7_API_KEY" | \
  jq -r '.content'
```

## Query Parameters

### Libraries Search (`/api/v2/libraries`)

| Parameter | Type | Description |
|-----------|------|-------------|
| `q` | string | **Required.** Search query for library name. |
| `limit` | integer | Max results (default: 10). |

### Context Retrieval (`/api/v2/context`)

| Parameter | Type | Description |
|-----------|------|-------------|
| `libraryId` | string | **Required.** Library ID from search (e.g., `/owner/repo` or `/owner/repo/v1.0.0`). |
| `query` | string | **Required.** Natural language query about the documentation topic. |

## Library ID Format

| Format | Example | Use Case |
|--------|---------|----------|
| Owner/Repo | `/vercel/next.js` | Latest version |
| With version | `/vercel/next.js/v15.1.8` | Specific version |

## Response Fields

### Library Search Response

| Field | Type | Description |
|-------|------|-------------|
| `id` | string | Library identifier for context queries |
| `name` | string | Package/library name |
| `description` | string | Library description |
| `language` | string | Primary programming language |
| `stars` | integer | GitHub stars |
| `versions` | array | Available indexed versions |

### Context Response

| Field | Type | Description |
|-------|------|-------------|
| `id` | string | Document identifier |
| `title` | string | Document title |
| `content` | string | Documentation content |
| `metadata.source` | string | Source URL |
| `metadata.library` | string | Library ID |
| `metadata.version` | string | Library version |

## Error Handling

| Status | Meaning | Action |
|--------|---------|--------|
| `200` | Success | Process normally |
| `202` | Accepted - Library not finalized | Wait and retry |
| `301` | Moved - Library redirected | Use new ID from `redirectUrl` |
| `400` | Bad Request | Check query parameters |
| `401` | Unauthorized - Invalid API key | Verify key (starts with `ctx7sk`) |
| `403` | Forbidden | Check library access permissions |
| `404` | Not Found | Verify the library ID |
| `422` | Unprocessable - Library too large/no code | Try different library |
| `429` | Too Many Requests | Wait for `Retry-After` header |
| `500` | Internal Server Error | Retry with backoff |
| `503` | Service Unavailable | Retry later |

## Rate Limits

- **Without API key**: Low rate limits, no custom configuration
- **With API key**: Higher limits based on plan
- View usage at https://context7.com/dashboard

## Best Practices

1. **Be specific with queries**: Use detailed natural language
   ```bash
   # Good
   curl -s ".../context?...&query=How%20to%20implement%20authentication%20with%20middleware"
   
   # Less optimal
   curl -s ".../context?...&query=auth"
   ```

2. **Cache responses**: Documentation updates infrequently, cache for hours or days

3. **Handle rate limits**: Implement exponential backoff for 429 errors

4. **Use specific versions**: Pin to exact versions for consistent results
   ```bash
   curl -s ".../context?libraryId=/vercel/next.js/v15.1.8&query=..."
   ```

Always check that `CONTEXT7_API_KEY` is set before making requests.

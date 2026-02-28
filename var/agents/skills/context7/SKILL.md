---
name: context7
description: Retrieve up-to-date documentation context for libraries using the Context7 API.
---

Documentation context retrieval using the Context7 API.

## Prerequisites

Requires `CONTEXT7_API_KEY` environment variable to be set. API keys start with `ctx7sk`.

## API Endpoint

```
GET https://context7.com/api/v2/context
```

Retrieve documentation context for a specific library.

## Get Context

Retrieve documentation context for a specific library.

**Request**:
```bash
curl -s "https://context7.com/api/v2/context?libraryId=/facebook/react&query=useEffect" \
  -H "Authorization: Bearer $CONTEXT7_API_KEY"
```

**Response**:
Plain text/markdown format with code examples and source references:

```markdown
### Perform Side Effects in React Function Components using useEffect Hook

Source: https://context7.com/facebook/react/llms.txt

The `useEffect` hook enables functional components to perform side effects...

\`\`\`jsx
import { useState, useEffect } from 'react';

function UserProfile({ userId }) {
  // ... code example ...
}
\`\`\`

--------------------------------

### React > Hooks > useEffect

Source: https://context7.com/facebook/react/llms.txt

The `useEffect` hook performs side effects in function components...
```

## Example Usage

```bash
# Get documentation for React useEffect hook
curl -s "https://context7.com/api/v2/context?libraryId=/facebook/react&query=useEffect" \
  -H "Authorization: Bearer $CONTEXT7_API_KEY"

# Get documentation for a specific concept
curl -s "https://context7.com/api/v2/context?libraryId=/facebook/react&query=useState%20hook" \
  -H "Authorization: Bearer $CONTEXT7_API_KEY"

# Save to file for reference
curl -s "https://context7.com/api/v2/context?libraryId=/facebook/react&query=hooks" \
  -H "Authorization: Bearer $CONTEXT7_API_KEY" > react-hooks.md

# Search with URL-encoded query
curl -s "https://context7.com/api/v2/context?libraryId=/vercel/next.js&query=app%20router" \
  -H "Authorization: Bearer $CONTEXT7_API_KEY"
```

## Query Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `libraryId` | string | **Required.** Library identifier (e.g., `/facebook/react`, `/vercel/next.js`). |
| `query` | string | **Required.** Natural language query about the documentation topic. |

## Library ID Format

Library IDs follow the pattern `/owner/repo`:

| Example | Description |
|---------|-------------|
| `/facebook/react` | React library |
| `/vercel/next.js` | Next.js framework |

## Response Format

### Context Response

The `/api/v2/context` endpoint returns plain text in markdown format with the following structure:

- **Multiple sections** separated by `--------------------------------`
- Each section contains:
  - A **heading** (e.g., `### Component Name`)
  - A **Source** URL
  - **Description text** explaining the concept
  - **Code examples** in fenced code blocks

Example structure:
```markdown
### Topic Title

Source: https://context7.com/owner/repo/llms.txt

Description text explaining the concept...

\`\`\`language
// Code example
\`\`\`

--------------------------------

### Another Topic

Source: https://github.com/owner/repo/blob/...

More documentation...
```

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

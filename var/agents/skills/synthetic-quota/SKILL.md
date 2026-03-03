---
name: synthetic-quota
description: Check Synthetic API usage quotas and limits.
---

Retrieve information about your Synthetic API usage quotas and limits.

## Prerequisites

- `SYNTHETIC_API_KEY` environment variable with a valid Synthetic API key

**Check before use:**
```bash
[ -z "$SYNTHETIC_API_KEY" ] && echo "Error: SYNTHETIC_API_KEY not set" || echo "OK: SYNTHETIC_API_KEY is set"
```

**Note:** Never hardcode `SYNTHETIC_API_KEY`. Always use the environment variable.

## API Endpoint

```
GET https://api.synthetic.new/v2/quotas
```

**Note**: `/quotas` requests do not count against your subscription limits.

## Check Quota

Retrieve current quota information.

**Request**:
```bash
curl -s https://api.synthetic.new/v2/quotas \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY"
```

**Response**:
```json
{
  "subscription": {
    "limit": 135,
    "requests": 0,
    "renewsAt": "2025-09-21T14:36:14.288Z"
  }
}
```

## Response Fields

| Field | Type | Description |
|-------|------|-------------|
| `subscription.limit` | number | Total request quota for the current period |
| `subscription.requests` | number | Number of requests used in the current period |
| `subscription.renewsAt` | string | ISO 8601 timestamp when the quota renews |

**Tip:** Run `date` to check the current datetime when comparing against `renewsAt` timestamps.

## Example Usage

```bash
# Check quota with formatted output
curl -s https://api.synthetic.new/v2/quotas \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY" | jq .

# Get remaining requests
curl -s https://api.synthetic.new/v2/quotas \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY" | jq '.subscription.limit - .subscription.requests'

# Check if quota renews soon (within 24 hours)
curl -s https://api.synthetic.new/v2/quotas \
  -H "Authorization: Bearer $SYNTHETIC_API_KEY" | jq '.subscription.renewsAt'
```

## Alias

Add this to your `~/.bashrc` or `~/.zshrc`:

```bash
alias synthetic_quota='curl -s https://api.synthetic.new/v2/quotas \
  -H "Authorization: Bearer ${SYNTHETIC_API_KEY}" \
  | jq --color-output .'
```

## Error Handling

The API will return appropriate HTTP status codes:
- `200` - Success
- `401` - Invalid or missing API key
- `429` - Rate limit exceeded
- `500` - Server error

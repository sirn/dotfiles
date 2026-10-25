---
name: models-dev
description: Query LLM model pricing and information from models.dev API. Use when user asks about LLM pricing, model costs, or which models are available on specific providers.
---

Query LLM model pricing and availability from models.dev.

## Data Source

```
GET https://models.dev/api.json
```

## Prerequisites

- `curl` to fetch the API data
- `jaq` to query and filter the JSON data

## Cache Management

The API data is cached at `~/.cache/agents/models.json` and refreshed when older than 24 hours.

**Update cache (if needed):**

```bash
CACHE_FILE="$HOME/.cache/agents/models.json"
mkdir -p "$(dirname "$CACHE_FILE")"

if [[ ! -f "$CACHE_FILE" ]] || [[ -n "$(find "$CACHE_FILE" -mtime +1 2>/dev/null)" ]]; then
  curl -fsS "https://models.dev/api.json" > "$CACHE_FILE"
fi
```

## Data Structure

The API is organized by provider:

- **Top level**: Provider IDs (e.g., `google`, `anthropic`, `openai`, `fireworks-ai`).
- **Provider fields**: `{id, name, models, doc, env, npm}`.
- **`models`**: Object mapping model IDs to model definitions.

**Example:**

```json
{
  "google": {
    "id": "google",
    "name": "Google",
    "models": {
      "gemini-3.5-flash": {
        "name": "Gemini 3.5 Flash",
        "cost": { "input": 1.5, "output": 9 },
        "limit": { "context": 1048576, "output": 65536 }
      }
    }
  }
}
```

## Common Queries

### List All Providers

```bash
CACHE_FILE="$HOME/.cache/agents/models.json"
jaq 'keys[]' "$CACHE_FILE"
```

### Get Pricing for a Specific Model

```bash
CACHE_FILE="$HOME/.cache/agents/models.json"
PROVIDER="google"
MODEL="gemini-3.5-flash"

jaq --arg p "$PROVIDER" --arg m "$MODEL" '
  .[$p].models[$m] | {
    name: .name,
    input_cost: .cost.input,
    output_cost: .cost.output,
    context_limit: .limit.context
  }
' "$CACHE_FILE"
```

### Find Models by Provider

```bash
CACHE_FILE="$HOME/.cache/agents/models.json"
PROVIDER="fireworks-ai"

jaq --arg p "$PROVIDER" '
  .[$p].models | to_entries | map({
    id: .key,
    name: .value.name,
    cost: .value.cost
  }) | sort_by(.name)
' "$CACHE_FILE"
```

### Search Models by Name Across All Providers

```bash
CACHE_FILE="$HOME/.cache/agents/models.json"
SEARCH="gemini flash"

jaq --arg s "$(echo "$SEARCH" | tr '[:upper:]' '[:lower:]')" '
  ($s | split(" ")) as $terms |
  to_entries | map(.key as $provider | .value.models | to_entries | map({
    provider: $provider,
    id: .key,
    name: .value.name,
    cost: .value.cost,
    limits: .value.limit
  })) | flatten | map(select(
    (.name | ascii_downcase) as $name |
    all($terms[]; $name | contains(.))
  ))
' "$CACHE_FILE"
```

### Find Providers Offering a Specific Model

```bash
CACHE_FILE="$HOME/.cache/agents/models.json"
MODEL_ID="gemini-3.5-flash"

jaq --arg m "$MODEL_ID" '
  to_entries | map(select(.value.models[$m])) | map({
    provider: .key,
    name: .value.name,
    model: .value.models[$m]
  })
' "$CACHE_FILE"
```

### List All Models with Pricing

```bash
CACHE_FILE="$HOME/.cache/agents/models.json"

jaq '
  to_entries | map(.key as $provider | .value.models | to_entries | map({
    provider: $provider,
    id: .key,
    name: .value.name,
    input_cost: .value.cost.input,
    output_cost: .value.cost.output
  })) | flatten | sort_by(.provider, .name) | .[:50]
' "$CACHE_FILE"
```

## Example Usage

**How much does Gemini 3 Flash cost?**

```bash
CACHE_FILE="$HOME/.cache/agents/models.json"

# Ensure cache is fresh
if [[ ! -f "$CACHE_FILE" ]] || [[ -n "$(find "$CACHE_FILE" -mtime +1 2>/dev/null)" ]]; then
  mkdir -p "$(dirname "$CACHE_FILE")"
  curl -fsS "https://models.dev/api.json" > "$CACHE_FILE"
fi

# Search for Gemini Flash models
jaq '
  to_entries | map(.key as $provider | .value.models | to_entries | map({
    provider: $provider,
    id: .key,
    name: .value.name,
    input_cost: .value.cost.input,
    output_cost: .value.cost.output,
    context: .value.limit.context
  })) | flatten | map(select(.name | ascii_downcase | contains("gemini") and contains("flash")))
' "$CACHE_FILE"
```

**What models are available on Fireworks?**

```bash
CACHE_FILE="$HOME/.cache/agents/models.json"

# Ensure cache is fresh
if [[ ! -f "$CACHE_FILE" ]] || [[ -n "$(find "$CACHE_FILE" -mtime +1 2>/dev/null)" ]]; then
  mkdir -p "$(dirname "$CACHE_FILE")"
  curl -fsS "https://models.dev/api.json" > "$CACHE_FILE"
fi

# Get Fireworks AI models
jaq '
  .["fireworks-ai"].models | to_entries | map({
    id: .key,
    name: .value.name,
    input_cost: .value.cost.input,
    output_cost: .value.cost.output,
    context: .value.limit.context
  }) | sort_by(.name)
' "$CACHE_FILE"
```

**What is the cheapest model for a given context size?**

```bash
CACHE_FILE="$HOME/.cache/agents/models.json"
MIN_CONTEXT=128000

jaq --argjson min_ctx "$MIN_CONTEXT" '
  to_entries | map(.key as $provider | .value.models | to_entries | map({
    provider: $provider,
    id: .key,
    name: .value.name,
    input_cost: .value.cost.input,
    context: .value.limit.context
  })) | flatten | map(select((.context // 0) >= $min_ctx and .input_cost != null and .input_cost > 0)) | sort_by(.input_cost) | .[:10]
' "$CACHE_FILE"
```

## Response Fields

| Field | Type | Description |
| --- | --- | --- |
| `id` | string | Provider ID (e.g., `google`, `fireworks-ai`) |
| `name` | string | Provider display name |
| `models.<id>.name` | string | Human-readable model name |
| `models.<id>.cost.input` | number | Input price per 1M tokens |
| `models.<id>.cost.output` | number | Output price per 1M tokens |
| `models.<id>.cost.cache_read` | number | Cached input price per 1M tokens (if supported) |
| `models.<id>.limit.context` | number | Maximum context window (tokens) |
| `models.<id>.limit.output` | number | Maximum output tokens |

## Error Handling

| Issue                | Resolution                       |
| -------------------- | -------------------------------- |
| Cache file missing   | Re-fetch from API                |
| Cache older than 24h | Re-fetch from API                |
| Provider not found   | Check provider ID spelling       |
| Model not found      | Check model ID or search by name |

## Cost Units

All prices are in USD per 1 million tokens.

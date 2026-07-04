# Remote Models Extension

Dynamically fetch and register custom model lists from OpenAI-style endpoints in the Pi Coding Agent.

## Overview

The Remote Models extension allows the Pi Coding Agent to dynamically load, map, and register provider and model configurations from one or more OpenAI-style `/v1/models` endpoints at startup.

Instead of hard-coding external model configurations or manually maintaining lists of available custom models in files, this extension queries the remote endpoint on agent startup, converts the remote model details into Pi-compatible `ProviderModelConfig` structures, and dynamically registers them as providers with the coding agent.

When a remote model provides hints pointing to existing configurations (such as those from `@earendil-works/pi-ai`), the extension inherits rich metadata, modalities, and capabilities from those definitions. If no hints are present, the extension sets up a baseline model configuration using raw data and custom config mappings.

## How It Works

At extension startup, the following sequence runs:

1. **Load Configuration**: The extension reads its configuration file from `~/.pi/agent/custom/remote-models/config.json` using a custom, dependency-free caching helper (`memoizeByStat`). This helper checks the file's modification time, size, and an FNV-1a content hash to detect changes without unnecessary reads. Each top-level key in the config object represents a target provider. If a provider config contains a valid `baseUrl` and `apiKeyEnv`, and uses an optional preset (such as `type: "plexus"`), those preset field mappings are merged.

2. **Fetch Models**: For each active provider configuration, the extension builds the target URL `${baseUrl}/v1/models` and fetches the list of available models. It passes the API key via an `Authorization: Bearer ${process.env[apiKeyEnv]}` header. header. To prevent hanging processes, the fetch uses a 30-second abort timeout (`AbortSignal.timeout(30_000)`). Re-fetching is protected by an in-memory cache (`modelsCache`) with a TTL of 300,000 milliseconds (5 minutes). This prevents redundant HTTP calls on consecutive extension reloads.

3. **Map Each Model**: For each returned model, the extension calls `toProviderModel()` to construct a `ProviderModelConfig`:
   - It reads `apiTypeField` to determine the streaming API style to use. It looks up the correct API type and relative path mapping (with a fallback to `defaultApiType` or `openai-completions`).
   - It checks the remote model object for `piProviderField` and `piModelField` hints (e.g., `"pi_provider"` and `"pi_model"`). If both are present, it looks up the base model definition using `@earendil-works/pi-ai`'s `getModel()`.
   - If a matching `pi-ai` base model exists, the registered model inherits its metadata, reasoning capability, `thinkingLevelMap`, input modalities, context window size, max tokens, and compatibility fields (`compat`).
   - If pricing information is supplied by the remote endpoint under the defined pricing paths, these values override the inherited `pi-ai` costs only if they are non-zero.
   - For models without a `pi-ai` hint, the extension sets up standard defaults (e.g., input modalities are filtered to only `["text", "image"]`) and applies the optional `unhandledThinkingLevelMap` (for example, exposing `"xhigh": "xhigh"`).

4. **Register Provider**: Once all model configurations for a provider are fetched and mapped, the extension builds a final `ProviderConfig` object. It sets the provider API key to reference the environment variable name (e.g., `"$${config.apiKeyEnv}"` which resolves to the literal name prefixed with `$` so Pi reads it at runtime) and registers the completed provider.

## Configuration

The extension is configured via a JSON file at: `~/.pi/agent/custom/remote-models/config.json`

### Provider Fields

Each top-level key in the configuration represents a provider name. The following fields can be configured for each provider:

`baseUrl` : `string` : The base URL of the remote API endpoint (required).

`apiKeyEnv` : `string` : Name of the env variable holding the API key (required).

`type` : `string` : Optional preset type (e.g., `"plexus"`). Fills in defaults.

`apiTypeField` : `string` : Remote field naming the preferred API (e.g., `"preferred_api"`).

`apiTypeMappings` : `object` : Maps an API type string to `{ api, path }` (e.g., `openai-completions`).

`defaultApiType` : `string` : Fallback key into `apiTypeMappings` if none is reported.

`piProviderField` : `string` : Remote field giving the `pi-ai` provider name hint.

`piModelField` : `string` : Remote field giving the `pi-ai` model name hint.

`pricingConvention` : `"perToken"` | `"perMillion"` : How prices are parsed. Defaults to `"perToken"`.

`pricingFieldMappings` : `object` : Dot-notation paths to input, output, cache read/write costs.

`unhandledThinkingLevelMap` : `object` : Thinking levels map for models without `pi-ai` hints.

### The Plexus Preset (`type: "plexus"`)

To reduce manual configuration, the `"plexus"` preset pre-configures common settings for Plexus proxies. When `type: "plexus"` is set, the following properties are automatically applied:

- **API Type Field**: `"preferred_api"`
- **API Type Mappings**:
  - `chat_completions` &rarr; `{ api: "openai-completions", path: "/v1" }`
  - `messages` &rarr; `{ api: "anthropic-messages" }`
  - `responses` &rarr; `{ api: "openai-responses", path: "/v1" }`
  - `gemini` &rarr; `{ api: "google-generative-ai", path: "/v1beta" }`
- **Default API Type**: `"chat_completions"`
- **Hint Fields**: `piProviderField` is set to `"pi_provider"`, and `piModelField` is set to `"pi_model"`
- **Pricing Mappings**:
  - input: `"pricing.prompt"`
  - output: `"pricing.completion"`
  - cacheRead: `"pricing.input_cache_read"`
  - cacheWrite: `"pricing.input_cache_write"`
- **Unhandled Thinking Map**: `{ xhigh: "xhigh" }`

### Configuration Example

Here is an example configuration using the `"plexus"` preset and adding a raw, custom provider:

```json
{
  "my-plexus-provider": {
    "baseUrl": "https://api.plexus.example.com",
    "apiKeyEnv": "PLEXUS_API_KEY",
    "type": "plexus"
  },
  "custom-provider": {
    "baseUrl": "https://api.custom.com",
    "apiKeyEnv": "CUSTOM_API_KEY",
    "apiTypeField": "api_style",
    "apiTypeMappings": {
      "standard": { "api": "openai-completions", "path": "/v1" }
    },
    "defaultApiType": "standard",
    "pricingConvention": "perMillion",
    "pricingFieldMappings": {
      "input": "costs.prompt_cost",
      "output": "costs.completion_cost"
    }
  }
}
```

## Architecture & Files

The extension's code is structured as follows:

```
remote-models/
├── index.ts              # Entry point: config, fetch, map, register
├── lib/
│   ├── cache.ts          # Stat-and-hash file cache (memoizeByStat)
│   └── perf.ts           # Optional perf instrumentation (measure)
└── README.md             # This file
```

### File Details

- **`index.ts`**: The central driver of the extension. It exports an asynchronous factory function as its default export. It coordinates configuration reading, parallel endpoint fetching, conversion of remote models into Pi model configurations, and dynamic registration of the providers with the Pi coding agent.

- **`lib/cache.ts`**: Contains `memoizeByStat()`, a cache helper that optimizes file reads. It compares both the filesystem stat info (`mtimeMs` and `size`) and evaluates a 32-bit FNV-1a non-cryptographic hash of the file content. This dual-check guarantees that changes are detected even if the modification timestamp is coarse or the file size remains the same (e.g. swap of characters).

- **`lib/perf.ts`**: Defines `measure()`, an execution time tracking helper. To eliminate overhead during normal runs, it is a no-op unless the `PI_EXTENSION_PERF` environment variable is set to `"1"` or `"true"`.

## Dependencies & Pi API Integration

### External Dependencies

The extension integrates with:

- **`@earendil-works/pi-coding-agent`**: Uses types like `ExtensionAPI`, `ProviderConfig`, and `ProviderModelConfig`.
- **`@earendil-works/pi-ai`**: Uses `getModel()` to look up base model information for inheriting rich configurations.
- **Node.js runtime APIs**: Imports `os` to locate the user's home directory and `path` for config file resolution.
- **Web Standards**: Utilizes global `fetch` and `AbortSignal.timeout` to handle HTTP requests.

### Pi API Call Sites

The extension integrates with the Pi coding-agent runtime exclusively via a single API call made within `index.ts`. It registers no extra commands, hooks, tools, message types, status-bar items, or UI renderers.

The exact integration point is:

```typescript
pi.registerProvider(result.value.configKey, result.value.providerConfig);
```

This call dynamically registers the dynamically fetched models and API details under the computed provider key (matching the top-level configuration key).

## Notable Implementation Details

### URL Resolution (`resolveUrl`)

To reliably build endpoint URLs, `resolveUrl(baseUrl, subPath)` is implemented with robust boundary handling:

- If `subPath` is already an absolute HTTP/HTTPS URL, it returns it directly.
- It ensures `baseUrl` ends with a trailing slash and strips any leading slashes from `subPath` to avoid double slashes or protocol-relative URL bugs.
- If `subPath` is empty, it returns `baseUrl` untouched.

### Pricing Parsing (`parsePricing`)

Pricing parsing converts values into a standard "cost per million tokens" format required by Pi:

- It returns `0` for null, undefined, or any non-finite numeric values.
- If `pricingConvention` is `"perToken"` (default), it multiplies the numeric value by `1,000,000`.
- If `pricingConvention` is `"perMillion"`, it uses the numeric value as-is, skipping any multiplication.

### Modality Filtering (`filterInputModalities`)

When a model doesn't specify a `pi-ai` base model to inherit from, its input capabilities must be resolved safely:

- Raw models are filtered via `filterInputModalities` to include only `"text"` and `"image"`. Any unsupported modalities like audio or video are discarded.
- If no valid modalities remain after filtering, it defaults to `["text"]`.
- Conversely, if a remote model hints at a known `pi-ai` base, raw input modalities are not filtered; instead, the `pi-ai` base model's own `input` array is used directly and unaltered.

### Caching and Resilience

- **HTTP Cache**: The endpoint response list is cached in-memory (`modelsCache`) with a TTL of 300,000 milliseconds (5 minutes). This ensures fast agent startup on subsequent runs. If the endpoint is down or returns a non-2xx code, the cache is not poisoned; instead, the fetch fails gracefully, keeping any existing valid cached entry.
- **Parallelization**: Fetches across multiple providers run concurrently using `Promise.allSettled()`. A single slow or offline endpoint cannot block other providers from successfully resolving and registering.
- **Performance Diagnostics**: When the environment variable `PI_EXTENSION_PERF=1` is configured, timing details for `remote-models.loadConfig` and model-fetching actions are logged to stderr using `console.warn`, enabling easy troubleshooting of slow startup.

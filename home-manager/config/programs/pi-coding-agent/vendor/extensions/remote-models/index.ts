/**
 * Remote Models Extension for Pi Coding Agent
 *
 * Fetches models dynamically from one or more remote /v1/models endpoints at
 * startup and registers them as Pi providers. Each named provider entry in the
 * config defines a baseUrl, apiKeyEnv, and mapping logic.
 *
 * A `type` preset (e.g., "plexus") fills in api type mappings, pricing field
 * mappings, and pi-ai hint field names so that common backends need minimal
 * config.
 *
 * Each model from the remote endpoint carries its own `api` and `baseUrl`,
 * so a single provider can contain models across different API types (chat
 * completions, messages, responses, generative).
 *
 * When a model hints at `piProvider`/`piModel`, the matching pi-ai model
 * definition is used as the base config (input modalities, costs, compat),
 * with remote-reported pricing taking precedence when non-zero.
 *
 * Only "text" and "image" input modalities are included for remote-derived
 * models; pi-ai inherited inputs are passed through unfiltered.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import type {
  ProviderConfig,
  ProviderModelConfig,
} from "@earendil-works/pi-coding-agent";
import { getModel } from "@earendil-works/pi-ai";

import * as os from "node:os";
import * as path from "node:path";
import { memoizeByStat } from "./lib/cache.js";
import { measure } from "./lib/perf.js";

// ---------------------------------------------------------------------------
// Config types
// ---------------------------------------------------------------------------

interface PricingFieldMappings {
  /** Path to input/prompt cost (e.g., "pricing.prompt", "model_info.input_cost_per_token"). */
  input?: string;
  /** Path to output/completion cost (e.g., "pricing.completion", "pricing.output"). */
  output?: string;
  /** Path to cache-read cost (e.g., "pricing.input_cache_read", "pricing.cached_input"). */
  cacheRead?: string;
  /** Path to cache-write cost (e.g., "pricing.input_cache_write", "model_info.cache_creation_input_token_cost"). */
  cacheWrite?: string;
}

interface ApiTypeMapping {
  /** Pi API type for streaming (e.g., "openai-completions"). */
  api: string;
  /** Relative URL path from baseUrl (e.g., "/v1"). Empty means baseUrl directly. */
  path?: string;
}

interface RemoteProviderConfig {
  baseUrl: string;
  apiKeyEnv: string;

  /** Field name in the API model object that selects the api type (e.g., "preferred_api"). */
  apiTypeField?: string;
  /** Map apiType string → { api, path }. */
  apiTypeMappings?: Record<string, ApiTypeMapping>;
  /** Key into apiTypeMappings used when no apiType hint is on a model. */
  defaultApiType?: string;
  /** Field name for the pi-ai provider hint (e.g., "pi_provider"). */
  piProviderField?: string;
  /** Field name for the pi-ai model hint (e.g., "pi_model"). */
  piModelField?: string;

  /** Preset type — fills in defaults before user overrides. */
  type?: string;

  /** How the remote API reports pricing values.
   *  - "perToken" (default): values are per-token; multiply by 1M
   *  - "perMillion": values are already per-1M-token; use as-is
   */
  pricingConvention?: "perToken" | "perMillion";
  /** Dot-notation paths to pricing fields in the API model object.
   *  Omitted fields mean no remote source for that cost dimension.
   */
  pricingFieldMappings?: PricingFieldMappings;
  /** Applied ONLY to models with no resolvable pi-ai hint (piModel === null).
   *  Lets proxy-routed models expose xhigh without inheriting pi-ai's
   *  per-provider flags, which may not apply behind a multi-provider endpoint. */
  unhandledThinkingLevelMap?: Partial<
    Record<
      "off" | "minimal" | "low" | "medium" | "high" | "xhigh",
      string | null
    >
  >;
}

// ---------------------------------------------------------------------------
// Presets
// ---------------------------------------------------------------------------

const PRESETS: Record<string, Partial<RemoteProviderConfig>> = {
  plexus: {
    apiTypeField: "preferred_api",
    apiTypeMappings: {
      chat_completions: { api: "openai-completions", path: "/v1" },
      messages: { api: "anthropic-messages" },
      responses: { api: "openai-responses", path: "/v1" },
      gemini: { api: "google-generative-ai", path: "/v1beta" },
    },
    defaultApiType: "chat_completions",
    piProviderField: "pi_provider",
    piModelField: "pi_model",
    pricingFieldMappings: {
      input: "pricing.prompt",
      output: "pricing.completion",
      cacheRead: "pricing.input_cache_read",
      cacheWrite: "pricing.input_cache_write",
    },
    // Plexus routes one endpoint to many upstream providers, so we can't
    // safely inherit pi-ai's per-provider flags via pi_provider/pi_model
    // hints. Instead expose xhigh for models pi-ai doesn't know about.
    unhandledThinkingLevelMap: { xhigh: "xhigh" },
  },
};

function mergePreset(config: RemoteProviderConfig): RemoteProviderConfig {
  if (!config.type) return config;
  if (!PRESETS[config.type]) {
    console.warn(
      `[remote-models] Unknown preset type "${config.type}" — ignoring`,
    );
    return config;
  }
  const preset = PRESETS[config.type];
  return {
    ...preset,
    ...config,
    apiTypeMappings: { ...preset.apiTypeMappings, ...config.apiTypeMappings },
    defaultApiType: config.defaultApiType ?? preset.defaultApiType,
    pricingFieldMappings: {
      ...preset.pricingFieldMappings,
      ...config.pricingFieldMappings,
    },
  };
}

// ---------------------------------------------------------------------------
// API model shape
// ---------------------------------------------------------------------------

interface RemoteApiModel {
  id: string;
  name?: string;
  architecture?: {
    input_modalities?: string[];
  };
  pricing?: Record<string, unknown>;
  supported_parameters?: string[];
  context_length?: number | null;
  top_provider?: {
    max_completion_tokens?: number | null;
    context_length?: number | null;
  };
  [key: string]: unknown;
}

interface RemoteApiResponse {
  data: RemoteApiModel[];
}

// ---------------------------------------------------------------------------
// URL resolution
// ---------------------------------------------------------------------------

function resolveUrl(baseUrl: string, subPath: string): string {
  if (/^https?:\/\//.test(subPath)) return subPath;
  // Ensure baseUrl has a trailing slash for correct URL resolution
  const base = baseUrl.endsWith("/") ? baseUrl : `${baseUrl}/`;
  // Strip leading slash from subPath to avoid protocol-relative URLs
  const relative = subPath.startsWith("/") ? subPath.slice(1) : subPath;
  if (!relative) return baseUrl;
  return `${base}${relative}`;
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Resolve a dot-notation path (e.g., "pricing.prompt") against an object.
 *  Returns undefined if the path doesn't exist or encounters a non-object mid-path.
 */
function resolvePath(
  obj: Record<string, unknown>,
  path: string | undefined,
): unknown {
  if (!path) return undefined;
  const parts = path.split(".");
  let current: unknown = obj;
  for (const part of parts) {
    if (
      current === null ||
      current === undefined ||
      typeof current !== "object"
    ) {
      return undefined;
    }
    current = (current as Record<string, unknown>)[part];
  }
  return current;
}

/** Parse a pricing value and convert to per-million-token cost.
 *  @param convention - "perToken" (default): multiply by 1M; "perMillion": use as-is.
 */
function parsePricing(
  value: unknown,
  convention: "perToken" | "perMillion" = "perToken",
): number {
  if (value == null) return 0;
  const str = String(value);
  const num = Number(str);
  if (!Number.isFinite(num)) return 0;
  return convention === "perToken" ? num * 1_000_000 : num;
}

/** Filter input modalities to only "text" and "image", defaulting to ["text"]. */
function filterInputModalities(
  modalities: string[] | undefined,
): ("text" | "image")[] {
  if (!modalities) return ["text"];
  const filtered = modalities.filter((m) => m === "text" || m === "image") as (
    | "text"
    | "image"
  )[];
  return filtered.length > 0 ? filtered : ["text"];
}

/** Convert a remote API model to Pi's ProviderModelConfig.
 *  When piProvider/piModel hints are present, inherit defaults from pi-ai
 *  and let remote-reported pricing override only when non-zero.
 *  Sets `api` and `baseUrl` per-model so a single provider can span
 *  multiple API types.
 */
function toProviderModel(
  apiModel: RemoteApiModel,
  config: RemoteProviderConfig,
  api: string,
  modelBaseUrl: string,
): ProviderModelConfig {
  const rawModel = apiModel as Record<string, unknown>;
  const piProvider = resolvePath(rawModel, config.piProviderField) as
    | string
    | undefined;
  const piModelId = resolvePath(rawModel, config.piModelField) as
    | string
    | undefined;

  const piModel =
    piProvider && piModelId
      ? (getModel(piProvider as any, piModelId as any) ?? null)
      : null;

  const contextWindow =
    apiModel.context_length ??
    apiModel.top_provider?.context_length ??
    piModel?.contextWindow ??
    4096;
  const maxTokens =
    apiModel.top_provider?.max_completion_tokens ??
    piModel?.maxTokens ??
    contextWindow;

  const fields = config.pricingFieldMappings;

  return {
    id: apiModel.id,
    name: apiModel.name ?? piModel?.name ?? apiModel.id,
    api: api as any,
    baseUrl: modelBaseUrl,
    reasoning:
      piModel?.reasoning ??
      apiModel.supported_parameters?.includes("reasoning") ??
      false,
    ...((piModel?.thinkingLevelMap ?? config.unhandledThinkingLevelMap) && {
      thinkingLevelMap:
        piModel?.thinkingLevelMap ?? config.unhandledThinkingLevelMap,
    }),
    input:
      piModel?.input ??
      filterInputModalities(apiModel.architecture?.input_modalities),
    cost: {
      input:
        parsePricing(
          resolvePath(rawModel, fields?.input),
          config.pricingConvention,
        ) ||
        piModel?.cost.input ||
        0,
      output:
        parsePricing(
          resolvePath(rawModel, fields?.output),
          config.pricingConvention,
        ) ||
        piModel?.cost.output ||
        0,
      cacheRead:
        parsePricing(
          resolvePath(rawModel, fields?.cacheRead),
          config.pricingConvention,
        ) ||
        piModel?.cost.cacheRead ||
        0,
      cacheWrite:
        parsePricing(
          resolvePath(rawModel, fields?.cacheWrite),
          config.pricingConvention,
        ) ||
        piModel?.cost.cacheWrite ||
        0,
    },
    contextWindow,
    maxTokens,
    ...(piModel?.compat && { compat: piModel.compat }),
  };
}

// ---------------------------------------------------------------------------
// Read config
// ---------------------------------------------------------------------------

const configPath = path.join(
  os.homedir(),
  ".pi/agent/custom/remote-models/config.json",
);

// TTL for cached remote model lists (ms). Re-fetched only after expiry or
// endpoint response error.
const REMOTE_MODELS_TTL_MS = 300_000;

async function loadProviders(): Promise<Record<string, RemoteProviderConfig>> {
  const result: Record<string, RemoteProviderConfig> = {};
  try {
    const cfg = await memoizeByStat(
      configPath,
      (content) => JSON.parse(content) as Record<string, unknown>,
    );
    if (!cfg || typeof cfg !== "object") return result;
    for (const [key, value] of Object.entries(cfg)) {
      if (
        typeof value === "object" &&
        value !== null &&
        typeof (value as Record<string, unknown>).baseUrl === "string" &&
        typeof (value as Record<string, unknown>).apiKeyEnv === "string"
      ) {
        result[key] = mergePreset(value as RemoteProviderConfig);
      }
    }
  } catch (e) {
    console.warn(
      `[remote-models] Failed to read config: ${e instanceof Error ? e.message : String(e)}`,
    );
  }
  return result;
}

// In-memory cache for fetched /v1/models payloads, keyed by endpoint URL.
interface CachedModels {
  fetchedAt: number;
  models: RemoteApiModel[];
}
const modelsCache = new Map<string, CachedModels>();

// ---------------------------------------------------------------------------
// Extension factory
// ---------------------------------------------------------------------------

const FALLBACK_MAPPING: ApiTypeMapping = { api: "openai-completions" };

/** Resolve an ApiTypeMapping for a model's apiType hint. */
function resolveApiTypeMapping(
  apiTypeValue: string | string[] | undefined,
  mappings: Record<string, ApiTypeMapping> | undefined,
  defaultApiType: string | undefined,
): ApiTypeMapping {
  const defaultMapping = mappings?.[defaultApiType ?? ""] ?? FALLBACK_MAPPING;
  if (!mappings || apiTypeValue === undefined) return defaultMapping;

  const candidates = Array.isArray(apiTypeValue)
    ? apiTypeValue
    : [apiTypeValue];
  for (const candidate of candidates) {
    const mapping = mappings[candidate];
    if (mapping) return mapping;
  }

  return defaultMapping;
}

async function fetchProviderModels(
  configKey: string,
  config: RemoteProviderConfig,
  debug: boolean,
): Promise<{ configKey: string; providerConfig: ProviderConfig } | null> {
  const apiKeyValue = process.env[config.apiKeyEnv];
  if (!apiKeyValue) {
    console.warn(
      `[remote-models] ${configKey}: ${config.apiKeyEnv} is not set — skipping`,
    );
    return null;
  }

  if (debug) console.warn(`[remote-models] ${configKey}: fetching models`);
  const endpointUrl = resolveUrl(config.baseUrl, "v1/models");

  // Serve from cache when fresh; avoids re-hitting the endpoint on every
  // extension reload within the TTL window.
  const cached = modelsCache.get(endpointUrl);
  let apiModels: RemoteApiModel[] | undefined;
  if (cached && Date.now() - cached.fetchedAt < REMOTE_MODELS_TTL_MS) {
    apiModels = cached.models;
  } else {
    try {
      const response = await fetch(endpointUrl, {
        headers: { Authorization: `Bearer ${apiKeyValue}` },
        signal: AbortSignal.timeout(30_000),
      });
      if (!response.ok) {
        console.warn(
          `[remote-models] ${configKey}: API returned ${response.status}: ${response.statusText}`,
        );
        return null;
      }
      const payload = (await response.json()) as RemoteApiResponse;
      if (!payload.data || !Array.isArray(payload.data)) {
        console.warn(
          `[remote-models] ${configKey}: API returned unexpected data format`,
        );
        return null;
      }
      apiModels = payload.data;
      modelsCache.set(endpointUrl, {
        fetchedAt: Date.now(),
        models: apiModels,
      });
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      console.warn(
        `[remote-models] ${configKey}: Failed to fetch models: ${message}`,
      );
      return null;
    }
  }

  const models: ProviderModelConfig[] = [];
  for (const apiModel of apiModels) {
    if (!apiModel.id) continue;

    const apiTypeValue = resolvePath(
      apiModel as Record<string, unknown>,
      config.apiTypeField,
    ) as string | string[] | undefined;

    const mapping = resolveApiTypeMapping(
      apiTypeValue,
      config.apiTypeMappings,
      config.defaultApiType,
    );

    models.push(
      toProviderModel(
        apiModel,
        config,
        mapping.api,
        resolveUrl(config.baseUrl, mapping.path ?? ""),
      ),
    );
  }

  if (models.length === 0) {
    console.warn(`[remote-models] ${configKey}: No models resolved — skipping`);
    return null;
  }

  const providerConfig: ProviderConfig = {
    baseUrl: config.baseUrl,
    apiKey: `$${config.apiKeyEnv}`,
    models,
  };
  return { configKey, providerConfig };
}

export default async function (pi: ExtensionAPI): Promise<void> {
  const providers = await measure("remote-models.loadConfig", loadProviders);
  if (Object.keys(providers).length === 0) return;

  const debug = process.env.PI_EXTENSION_PERF === "1";

  // Fetch all providers concurrently; each settles independently so a
  // single slow/erroring endpoint doesn't gate the others.
  const results = await Promise.allSettled(
    Object.entries(providers).map(([configKey, config]) =>
      fetchProviderModels(configKey, config, debug),
    ),
  );

  for (const result of results) {
    if (result.status !== "fulfilled" || !result.value) continue;
    pi.registerProvider(result.value.configKey, result.value.providerConfig);
  }
}

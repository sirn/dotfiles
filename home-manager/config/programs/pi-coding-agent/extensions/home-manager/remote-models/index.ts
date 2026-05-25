/**
 * Remote Models Extension for Pi Coding Agent
 *
 * Fetches models dynamically from one or more remote /v1/models endpoints at
 * startup and registers them as Pi providers. Each named provider entry in the
 * config defines a baseUrl, apiKeyEnv, and mapping logic.
 *
 * A `type` preset (e.g., "plexus") fills in hint field names, api type mappings,
 * and URL defaults so that common backends need minimal config.
 *
 * Provider IDs are derived from the config key plus an apiType suffix:
 *
 *   key="plexus" + chat_completions → plexus            (openai-completions)
 *   key="plexus" + messages         → plexus-messages   (anthropic-messages)
 *   key="plexus" + responses        → plexus-responses  (openai-responses)
 *   key="plexus" + gemini           → plexus-generative (google-generative-ai)
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

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";

// ---------------------------------------------------------------------------
// Config types
// ---------------------------------------------------------------------------

interface HintFields {
  /** Field name in the API model object that selects the api type (e.g., "preferred_api"). */
  apiType?: string;
  /** Field name for the pi-ai provider hint (e.g., "pi_provider"). */
  piProvider?: string;
  /** Field name for the pi-ai model hint (e.g., "pi_model"). */
  piModel?: string;
}

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
  /** Pi API type for streaming. */
  api: ProviderConfig["api"];
  /** Full URL or relative path from baseUrl. */
  url: string;
  /** Suffix appended to the config key to form the provider ID (e.g., "-messages"). */
  providerSuffix?: string;
}

interface RemoteProviderConfig {
  baseUrl: string;
  apiKeyEnv: string;

  hintFields?: HintFields;
  apiTypeMappings?: Record<string, ApiTypeMapping>;
  /** Key into apiTypeMappings used when no apiType hint is on a model. */
  defaultApiType?: string;

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
}

// ---------------------------------------------------------------------------
// Presets
// ---------------------------------------------------------------------------

const PRESETS: Record<string, Partial<RemoteProviderConfig>> = {
  plexus: {
    hintFields: {
      apiType: "preferred_api",
      piProvider: "pi_provider",
      piModel: "pi_model",
    },
    apiTypeMappings: {
      chat_completions: { api: "openai-completions", url: "/v1" },
      messages: {
        api: "anthropic-messages",
        url: "",
        providerSuffix: "-messages",
      },
      responses: {
        api: "openai-responses",
        url: "/v1",
        providerSuffix: "-responses",
      },
      gemini: {
        api: "google-generative-ai",
        url: "/v1beta",
        providerSuffix: "-generative",
      },
    },
    defaultApiType: "chat_completions",
    pricingFieldMappings: {
      input: "pricing.prompt",
      output: "pricing.completion",
      cacheRead: "pricing.input_cache_read",
      cacheWrite: "pricing.input_cache_write",
    },
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
    hintFields: { ...preset.hintFields, ...config.hintFields },
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
// Mapping resolution
// ---------------------------------------------------------------------------

interface ResolvedMapping {
  providerId: string;
  api: ProviderConfig["api"];
  url: string;
}

function resolveUrl(baseUrl: string, mappingUrl: string): string {
  if (/^https?:\/\//.test(mappingUrl)) return mappingUrl;
  // Ensure baseUrl has a trailing slash for correct URL resolution
  const base = baseUrl.endsWith("/") ? baseUrl : `${baseUrl}/`;
  // Strip leading slash from mappingUrl to avoid protocol-relative URLs
  const relative = mappingUrl.startsWith("/")
    ? mappingUrl.slice(1)
    : mappingUrl;
  if (!relative) return baseUrl;
  return `${base}${relative}`;
}

function resolveMapping(
  apiTypeValue: string | string[] | undefined,
  configKey: string,
  mappings: Record<string, ApiTypeMapping> | undefined,
  defaultApiType: string | undefined,
): ResolvedMapping | null {
  const defaultEntry = mappings?.[defaultApiType ?? ""];
  const fallback: ApiTypeMapping = defaultEntry ?? {
    api: "openai-completions",
    url: "/v1",
  };

  // No hint or no mappings defined — use default
  if (!mappings || apiTypeValue === undefined) {
    return {
      providerId: configKey + (fallback.providerSuffix ?? ""),
      api: fallback.api,
      url: fallback.url,
    };
  }

  const candidates = Array.isArray(apiTypeValue)
    ? apiTypeValue
    : [apiTypeValue];
  for (const candidate of candidates) {
    const mapping = mappings[candidate];
    if (mapping) {
      return {
        providerId: configKey + (mapping.providerSuffix ?? ""),
        api: mapping.api,
        url: mapping.url,
      };
    }
  }

  // Hint present but no mapping matched — caller should skip this model
  return null;
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

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

/** Convert a remote API model to Pi's ProviderModelConfig.
 *  When piProvider/piModel hints are present, inherit defaults from pi-ai
 *  and let remote-reported pricing override only when non-zero.
 */
function toProviderModel(
  apiModel: RemoteApiModel,
  config: RemoteProviderConfig,
): ProviderModelConfig {
  const { hintFields, pricingConvention, pricingFieldMappings } = config;
  const rawModel = apiModel as Record<string, unknown>;
  const piProvider = resolvePath(rawModel, hintFields?.piProvider) as
    | string
    | undefined;
  const piModelId = resolvePath(rawModel, hintFields?.piModel) as
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

  const fields = pricingFieldMappings;

  return {
    id: apiModel.id,
    name: apiModel.name ?? piModel?.name ?? apiModel.id,
    reasoning:
      piModel?.reasoning ??
      apiModel.supported_parameters?.includes("reasoning") ??
      false,
    ...(piModel?.thinkingLevelMap && {
      thinkingLevelMap: piModel.thinkingLevelMap,
    }),
    input:
      piModel?.input ??
      filterInputModalities(apiModel.architecture?.input_modalities),
    cost: {
      input:
        parsePricing(resolvePath(rawModel, fields?.input), pricingConvention) ||
        piModel?.cost.input ||
        0,
      output:
        parsePricing(
          resolvePath(rawModel, fields?.output),
          pricingConvention,
        ) ||
        piModel?.cost.output ||
        0,
      cacheRead:
        parsePricing(
          resolvePath(rawModel, fields?.cacheRead),
          pricingConvention,
        ) ||
        piModel?.cost.cacheRead ||
        0,
      cacheWrite:
        parsePricing(
          resolvePath(rawModel, fields?.cacheWrite),
          pricingConvention,
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

let providers: Record<string, RemoteProviderConfig> = {};
try {
  if (fs.existsSync(configPath)) {
    const cfg = JSON.parse(fs.readFileSync(configPath, "utf-8"));
    if (typeof cfg === "object" && cfg !== null) {
      for (const [key, value] of Object.entries(cfg)) {
        if (
          typeof value === "object" &&
          value !== null &&
          typeof (value as Record<string, unknown>).baseUrl === "string" &&
          typeof (value as Record<string, unknown>).apiKeyEnv === "string"
        ) {
          providers[key] = mergePreset(value as RemoteProviderConfig);
        }
      }
    }
  }
} catch (e) {
  console.warn(
    `[remote-models] Failed to read config: ${e instanceof Error ? e.message : String(e)}`,
  );
}

// ---------------------------------------------------------------------------
// Extension factory
// ---------------------------------------------------------------------------

export default async function (pi: ExtensionAPI): Promise<void> {
  if (Object.keys(providers).length === 0) return;

  for (const [configKey, config] of Object.entries(providers)) {
    const apiKeyValue = process.env[config.apiKeyEnv];
    if (!apiKeyValue) {
      console.warn(
        `[remote-models] ${configKey}: ${config.apiKeyEnv} is not set — skipping`,
      );
      continue;
    }

    try {
      const response = await fetch(resolveUrl(config.baseUrl, "v1/models"), {
        headers: { Authorization: `Bearer ${apiKeyValue}` },
        signal: AbortSignal.timeout(30_000),
      });

      if (!response.ok) {
        console.warn(
          `[remote-models] ${configKey}: API returned ${response.status}: ${response.statusText}`,
        );
        continue;
      }

      const payload = (await response.json()) as RemoteApiResponse;

      if (!payload.data || !Array.isArray(payload.data)) {
        console.warn(
          `[remote-models] ${configKey}: API returned unexpected data format`,
        );
        continue;
      }

      // Group models by their resolved provider mapping
      const groups = new Map<
        string,
        { mapping: ResolvedMapping; models: ProviderModelConfig[] }
      >();
      for (const apiModel of payload.data) {
        if (!apiModel.id) continue;

        const apiTypeValue = resolvePath(
          apiModel as Record<string, unknown>,
          config.hintFields?.apiType,
        ) as string | string[] | undefined;
        const mapping = resolveMapping(
          apiTypeValue,
          configKey,
          config.apiTypeMappings,
          config.defaultApiType,
        );
        if (!mapping) continue;

        const existing = groups.get(mapping.providerId);
        if (existing) {
          if (
            existing.mapping.api !== mapping.api ||
            existing.mapping.url !== mapping.url
          ) {
            console.warn(
              `[remote-models] ${configKey}: provider ID collision on "${mapping.providerId}" with different api/url — skipping model ${apiModel.id}`,
            );
            continue;
          }
          existing.models.push(toProviderModel(apiModel, config));
        } else {
          groups.set(mapping.providerId, {
            mapping,
            models: [toProviderModel(apiModel, config)],
          });
        }
      }

      for (const [, entry] of groups) {
        const { mapping, models } = entry;
        const providerConfig: ProviderConfig = {
          baseUrl: resolveUrl(config.baseUrl, mapping.url),
          apiKey: config.apiKeyEnv,
          api: mapping.api,
          models,
        };
        pi.registerProvider(mapping.providerId, providerConfig);
      }
    } catch (error) {
      const message = error instanceof Error ? error.message : String(error);
      console.warn(
        `[remote-models] ${configKey}: Failed to fetch models: ${message}`,
      );
    }
  }
}

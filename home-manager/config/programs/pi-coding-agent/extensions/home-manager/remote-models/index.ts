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

interface ApiTypeMapping {
  /** Pi API type for streaming. */
  api: ProviderConfig["api"];
  /** Full URL or relative path from baseUrl. */
  url: string;
  /** Suffix appended to the config key to form the provider ID (e.g., "-messages"). */
  providerSuffix?: string;
}

interface DefaultMapping {
  api: ProviderConfig["api"];
  url: string;
}

interface RemoteProviderConfig {
  baseUrl: string;
  apiKeyEnv: string;

  hintFields?: HintFields;
  apiTypeMappings?: Record<string, ApiTypeMapping>;
  defaultMapping?: DefaultMapping;

  /** Preset type — fills in defaults before user overrides. */
  type?: string;
}

// ---------------------------------------------------------------------------
// Presets
// ---------------------------------------------------------------------------

const PLEXUS_PRESET: Partial<RemoteProviderConfig> = {
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
  defaultMapping: { api: "openai-completions", url: "/v1" },

};

const PRESETS: Record<string, Partial<RemoteProviderConfig>> = {
  plexus: PLEXUS_PRESET,
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
    defaultMapping: config.defaultMapping ?? preset.defaultMapping,
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
  pricing?: {
    prompt?: string;
    completion?: string;
    input_cache_read?: string;
    input_cache_write?: string;
  };
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
  defaultMapping: DefaultMapping | undefined,
): ResolvedMapping | null {
  const fallback: DefaultMapping = defaultMapping ?? {
    api: "openai-completions",
    url: "/v1",
  };

  // No hint or no mappings defined — use default
  if (!mappings || apiTypeValue === undefined) {
    return {
      providerId: configKey,
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

/** Parse a per-token pricing string (e.g. "7.5e-7") and convert to per-million-token cost. */
function parsePricing(value: string | null | undefined): number {
  if (!value) return 0;
  const num = Number(value);
  return Number.isFinite(num) ? num * 1_000_000 : 0;
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

/** Read a hint field from the API model by its configured field name. */
function readHint(model: RemoteApiModel, fieldName?: string): unknown {
  if (!fieldName) return undefined;
  return model[fieldName];
}

/** Convert a remote API model to Pi's ProviderModelConfig.
 *  When piProvider/piModel hints are present, inherit defaults from pi-ai
 *  and let remote-reported pricing override only when non-zero.
 */
function toProviderModel(
  apiModel: RemoteApiModel,
  hintFields: HintFields | undefined,
): ProviderModelConfig {
  const piProvider = readHint(apiModel, hintFields?.piProvider) as
    | string
    | undefined;
  const piModelId = readHint(apiModel, hintFields?.piModel) as
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
      input: parsePricing(apiModel.pricing?.prompt) || piModel?.cost.input || 0,
      output:
        parsePricing(apiModel.pricing?.completion) || piModel?.cost.output || 0,
      cacheRead:
        parsePricing(apiModel.pricing?.input_cache_read) ||
        piModel?.cost.cacheRead ||
        0,
      cacheWrite:
        parsePricing(apiModel.pricing?.input_cache_write) ||
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

        const apiTypeValue = readHint(apiModel, config.hintFields?.apiType) as
          | string
          | string[]
          | undefined;
        const mapping = resolveMapping(
          apiTypeValue,
          configKey,
          config.apiTypeMappings,
          config.defaultMapping,
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
          existing.models.push(toProviderModel(apiModel, config.hintFields));
        } else {
          groups.set(mapping.providerId, {
            mapping,
            models: [toProviderModel(apiModel, config.hintFields)],
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

/**
 * Plexus Models Extension for Pi Coding Agent
 *
 * Fetches models dynamically from the Plexus API at startup and registers them
 * across providers chosen by Plexus's `preferred_api` hint:
 *
 *   chat_completions → plexus            / openai-completions (default)
 *   messages         → plexus-messages   / anthropic-messages
 *   responses        → plexus-responses  / openai-responses
 *   gemini           → plexus-generative / google-generative-ai
 *
 * When Plexus advertises `pi_provider` / `pi_model`, the matching pi-ai model
 * definition is used as the base config (input modalities, costs, compat),
 * with Plexus-reported pricing taking precedence when non-zero.
 *
 * Only "text" and "image" input modalities are included.
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
// API types
// ---------------------------------------------------------------------------

interface PlexusApiModel {
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
  preferred_api?: string | string[];
  pi_provider?: string;
  pi_model?: string;
}

interface PlexusApiResponse {
  data: PlexusApiModel[];
}

// ---------------------------------------------------------------------------
// preferred_api → (provider, api, baseUrl suffix) mapping
// ---------------------------------------------------------------------------

interface ApiMapping {
  provider: string;
  api: ProviderConfig["api"];
  suffix: string;
}

const API_MAP: Record<string, ApiMapping> = {
  chat_completions: {
    provider: "plexus",
    api: "openai-completions",
    suffix: "/v1",
  },
  messages: {
    provider: "plexus-messages",
    api: "anthropic-messages",
    suffix: "",
  },
  responses: {
    provider: "plexus-responses",
    api: "openai-responses",
    suffix: "/v1",
  },
  gemini: {
    provider: "plexus-generative",
    api: "google-generative-ai",
    suffix: "/v1beta",
  },
};

const DEFAULT_MAPPING = API_MAP.chat_completions;

function resolveMapping(preferred: string | string[] | undefined): ApiMapping {
  if (!preferred) return DEFAULT_MAPPING;
  const candidates = Array.isArray(preferred) ? preferred : [preferred];
  for (const candidate of candidates) {
    const mapping = API_MAP[candidate];
    if (mapping) return mapping;
  }
  return DEFAULT_MAPPING;
}

const configPath = path.join(
  os.homedir(),
  ".pi/agent/custom/plexus-models/config.json",
);
let baseUrl: string | null = null;
try {
  if (fs.existsSync(configPath)) {
    const cfg = JSON.parse(fs.readFileSync(configPath, "utf-8"));
    if (typeof cfg.baseUrl === "string" && cfg.baseUrl.length > 0) {
      baseUrl = cfg.baseUrl;
    }
  }
} catch {
  // Config missing or invalid — extension will be a no-op
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/** Parse a per-token pricing string (e.g. "7.5e-7") and convert to per-million-token cost.
 * Pi expects cost values in dollars per million tokens,
 * but the Plexus/OpenRouter API returns dollars per token.
 */
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

/** Convert a Plexus API model to Pi's ProviderModelConfig.
 *  When pi_provider/pi_model hints are present, inherit defaults from pi-ai
 *  and let Plexus-reported pricing override only when non-zero.
 */
function toProviderModel(apiModel: PlexusApiModel): ProviderModelConfig {
  const piModel =
    apiModel.pi_provider && apiModel.pi_model
      ? (getModel(apiModel.pi_provider as any, apiModel.pi_model as any) ??
        null)
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
// Extension factory
// ---------------------------------------------------------------------------

export default async function (pi: ExtensionAPI): Promise<void> {
  if (!baseUrl) return;

  // Plexus proxies to multiple backends that disagree on field names and roles.
  // Normalize outgoing openai-completions requests so every backend can handle them:
  // 1. reasoning → reasoning_content: Fireworks rejects `reasoning` (400). Both accept `reasoning_content`.
  // 2. developer → system: synthetic/neuralwatt Qwen reject `developer` (400). `system` is universal.
  // 3. assistant.content: null → "": wafer-pass rejects `null` content on assistant messages (400).
  pi.on("before_provider_request", (event, ctx) => {
    const model = ctx.model;
    if (!model?.provider?.startsWith("plexus")) return;
    if (model.api !== "openai-completions") return;

    const payload = event.payload;

    if (
      !payload ||
      typeof payload !== "object" ||
      !Array.isArray((payload as Record<string, unknown>).messages)
    ) {
      return;
    }

    const normalized = JSON.parse(JSON.stringify(payload)) as Record<
      string,
      unknown
    >;
    const messages = normalized.messages as Array<Record<string, unknown>>;

    let changed = false;
    for (const message of messages) {
      // 1. developer → system
      //    synthetic/neuralwatt Qwen: 400 "Unexpected message role."
      if (message.role === "developer") {
        message.role = "system";
        changed = true;
      }

      // 2. assistant content: null → ""
      //    wafer-pass: 400 "messages[N].content cannot be null"
      if (message.role === "assistant" && message.content === null) {
        message.content = "";
        changed = true;
      }

      // 3. reasoning → reasoning_content
      //    Fireworks: 400 "Extra inputs are not permitted"
      if (
        message.reasoning !== undefined &&
        message.reasoning_content === undefined
      ) {
        message.reasoning_content = message.reasoning;
        delete message.reasoning;
        changed = true;
      }
    }

    if (changed) {
      return normalized;
    }
  });

  try {
    const response = await fetch(`${baseUrl}/v1/models`, {
      headers: { Authorization: `Bearer ${process.env.PLEXUS_API_KEY}` },
    });

    if (!response.ok) {
      console.warn(
        `[plexus-models] Plexus API returned ${response.status}: ${response.statusText}`,
      );
      return;
    }

    const payload = (await response.json()) as PlexusApiResponse;

    if (!payload.data || !Array.isArray(payload.data)) {
      console.warn(
        "[plexus-models] Plexus API returned unexpected data format",
      );
      return;
    }

    // Group models by their resolved provider mapping
    const groups = new Map<ApiMapping, ProviderModelConfig[]>();
    for (const apiModel of payload.data) {
      if (!apiModel.id) continue;
      const mapping = resolveMapping(apiModel.preferred_api);
      const existing = groups.get(mapping) ?? [];
      existing.push(toProviderModel(apiModel));
      groups.set(mapping, existing);
    }

    for (const [mapping, models] of groups) {
      const config: ProviderConfig = {
        baseUrl: `${baseUrl}${mapping.suffix}`,
        apiKey: "PLEXUS_API_KEY",
        api: mapping.api,
        models,
      };
      pi.registerProvider(mapping.provider, config);
    }
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    // Don't crash Pi if the fetch fails — just log and move on
    console.warn(
      `[plexus-models] Failed to fetch models from Plexus: ${message}`,
    );
  }
}

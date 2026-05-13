/**
 * Plexus Models Extension for Pi Coding Agent
 *
 * Fetches models dynamically from the Plexus API at startup and registers them
 * across 4 providers based on model ID prefix:
 *
 *   gpt-*     → plexus-responses  / openai-responses
 *   claude-*  → plexus-messages   / anthropic-messages
 *   gemini-*  → plexus-generative / google-generative-ai
 *   *other*   → plexus            / openai-completions (default)
 *
 * Only "text" and "image" input modalities are included.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import type {
  ProviderConfig,
  ProviderModelConfig,
} from "@mariozechner/pi-coding-agent";

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
// ---------------------------------------------------------------------------
// API types
// ---------------------------------------------------------------------------

interface PlexusApiModel {
  id: string;
  name: string;
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
  top_provider?: {
    max_completion_tokens?: number;
    context_length?: number;
  };
}

interface PlexusApiResponse {
  data: PlexusApiModel[];
}

// ---------------------------------------------------------------------------
// Family → (provider, api, baseUrl) mapping
// ---------------------------------------------------------------------------

interface FamilyMapping {
  provider: string;
  api: string;
  suffix: string;
}

const FAMILY_MAP: Record<string, FamilyMapping> = {
  claude: {
    provider: "plexus-messages",
    api: "anthropic-messages",
    suffix: "",
  },
  openai: {
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

const DEFAULT_MAPPING: FamilyMapping = {
  provider: "plexus",
  api: "openai-completions",
  suffix: "/v1",
};

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

/** Derive family from model ID prefix (gpt-*, claude-*, gemini-*). */
function getFamily(id: string): string {
  if (id.startsWith("gpt-")) return "openai";
  if (id.startsWith("claude-")) return "claude";
  if (id.startsWith("gemini-")) return "gemini";
  return "__default__";
}

/** Parse a pricing string like "7.5e-7" or "0.00000174" into a number. */
function parsePricing(value: string | null | undefined): number {
  if (!value) return 0;
  const num = Number(value);
  return Number.isFinite(num) ? num : 0;
}

/**
 * Filter input modalities to only "text" and "image"
 *
 */
function filterInputModalities(
  modalities: string[] | undefined,
): ("text" | "image")[] {
  if (!modalities) return ["text"];
  const filtered = modalities.filter((m) => m === "text" || m === "image") as (
    | "text"
    | "image"
  )[];
  // Ensure at least "text" is present
  return filtered.length > 0 ? filtered : ["text"];
}

/** Convert a Plexus API model to Pi's ProviderModelConfig. */
function toProviderModel(apiModel: PlexusApiModel): ProviderModelConfig {
  return {
    id: apiModel.id,
    name: apiModel.name,
    reasoning: apiModel.supported_parameters?.includes("reasoning") ?? false,
    input: filterInputModalities(apiModel.architecture?.input_modalities),
    cost: {
      input: parsePricing(apiModel.pricing?.prompt),
      output: parsePricing(apiModel.pricing?.completion),
      cacheRead: parsePricing(apiModel.pricing?.input_cache_read),
      cacheWrite: parsePricing(apiModel.pricing?.input_cache_write),
    },
    contextWindow:
      apiModel.top_provider?.context_length ??
      apiModel.top_provider?.max_completion_tokens ??
      4096,
    maxTokens:
      apiModel.top_provider?.max_completion_tokens ??
      apiModel.top_provider?.context_length ??
      4096,
  };
}

// ---------------------------------------------------------------------------
// Extension factory
// ---------------------------------------------------------------------------

export default async function (pi: ExtensionAPI): Promise<void> {
  if (!baseUrl) return;

  // Plexus proxies to multiple backends (Synthetic, Fireworks, etc.) that
  // disagree on the reasoning-content field name: Synthetic uses `reasoning`,
  // Fireworks uses `reasoning_content`. Fireworks rejects `reasoning` with
  // "Extra inputs are not permitted". Both backends accept `reasoning_content`,
  // so normalize outgoing requests to that field.
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
      pi.ui.notify?.(
        `Plexus API returned ${response.status}: ${response.statusText}`,
        "warning",
      );
      return;
    }

    const payload = (await response.json()) as PlexusApiResponse;

    if (!payload.data || !Array.isArray(payload.data)) {
      pi.ui.notify?.("Plexus API returned unexpected data format", "warning");
      return;
    }

    // Group models by provider
    const groups = new Map<string, ProviderModelConfig[]>();
    for (const apiModel of payload.data) {
      const family = getFamily(apiModel.id);
      const mapping = FAMILY_MAP[family] ?? DEFAULT_MAPPING;

      const existing = groups.get(mapping.provider) ?? [];
      existing.push(toProviderModel(apiModel));
      groups.set(mapping.provider, existing);
    }

    // Register each provider with its models
    for (const [provider, models] of groups) {
      // Determine the mapping for this provider (find first model's family mapping)
      const firstModelId = models[0]?.id ?? "";
      const family = getFamily(firstModelId);
      const mapping = FAMILY_MAP[family] ?? DEFAULT_MAPPING;

      const config: ProviderConfig = {
        baseUrl: `${baseUrl}${mapping.suffix}`,
        apiKey: "PLEXUS_API_KEY",
        api: mapping.api as ProviderConfig["api"],
        models,
      };

      pi.registerProvider(provider, config);
    }
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    // Don't crash Pi if the fetch fails — just log and move on
    console.warn(
      `[plexus-models] Failed to fetch models from Plexus: ${message}`,
    );
  }
}

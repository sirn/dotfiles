/**
 * Execution Policy Extension for Pi Coding Agent
 *
 * Combines plan-mode, delegate-mode, and shell-policy functionality.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import planMode from "./plan-mode.js";
import delegateMode from "./delegate-mode.js";
import shellPolicy from "./shell-policy.js";
import { setupModePromptInjection } from "./lib/execution-mode.js";

export default function (pi: ExtensionAPI) {
  planMode(pi);
  delegateMode(pi);
  shellPolicy(pi);
  setupModePromptInjection(pi);
}

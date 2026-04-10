/**
 * Execution Policy Extension for Pi Coding Agent
 *
 * Combines plan-mode and shell-policy functionality.
 */

import type { ExtensionAPI } from "@mariozechner/pi-coding-agent";
import planMode from "./plan-mode.js";
import shellPolicy from "./shell-policy.js";

export default function (pi: ExtensionAPI) {
  planMode(pi);
  shellPolicy(pi);
}

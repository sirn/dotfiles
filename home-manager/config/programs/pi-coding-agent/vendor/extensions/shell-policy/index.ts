/**
 * Shell Policy Extension for Pi Coding Agent
 *
 * Policy enforcement: tool_call hooks, command evaluation, auto-mode.
 */

import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import shellPolicy from "./shell-policy.js";

export default function (pi: ExtensionAPI) {
  shellPolicy(pi);
}

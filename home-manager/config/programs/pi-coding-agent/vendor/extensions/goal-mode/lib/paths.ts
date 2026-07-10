import * as os from "node:os";
import * as path from "node:path";
export const PI_AGENT_DIR = path.join(os.homedir(), ".pi/agent");
export const EXT_DIR = path.join(PI_AGENT_DIR, "custom/goal-mode");
export const PROMPTS_DIR = path.join(EXT_DIR, "prompts");
export const CONFIG_PATH = path.join(EXT_DIR, "config.json");

import * as os from "node:os";
import * as path from "node:path";
export const PI_AGENT_DIR = path.join(os.homedir(), ".pi/agent");
export const PLAN_DIR = path.join(PI_AGENT_DIR, "plans");
export const EXT_DIR = path.join(PI_AGENT_DIR, "custom/plan-mode");
export const PROMPTS_DIR = path.join(EXT_DIR, "prompts");

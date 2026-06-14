import * as os from "node:os";
import * as path from "node:path";

export const PI_AGENT_DIR = path.join(os.homedir(), ".pi/agent");
export const EXT_DIR = path.join(PI_AGENT_DIR, "custom/shell-policy");

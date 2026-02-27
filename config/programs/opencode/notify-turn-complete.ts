/* Notify OpenCode - send desktop notification when agent finishes a turn. */

import type { Plugin } from "@opencode-ai/plugin"

const toastifyBin = "__TOASTIFY_BIN__"

export const NotifyTurnComplete: Plugin = async ({ client, $ }) => {
  return {
    event: async ({ event }) => {
      if (event.type === "session.idle") {
        try {
          const sessionID = event.properties.sessionID
          const response = await client.session.messages({ path: { id: sessionID } })
          const messages = response.data ?? []
          const lastAssistant = messages.filter((m) => m.info.role === "assistant").pop()
          const textParts = (lastAssistant?.parts ?? [])
            .filter((p: any) => p.type === "text")
            .map((p: any) => p.text)
            .join(" ")
          const truncated = textParts.replace(/\n/g, " ").trim().slice(0, 200)
          const body = truncated || "OpenCode has finished their turn"
          await $`${toastifyBin} send "OpenCode" ${body}`
        } catch {
          await $`${toastifyBin} send "OpenCode" "OpenCode has finished their turn"`
        }
      }
    },
  }
}

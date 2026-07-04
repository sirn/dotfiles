# Notify Turn Complete Extension

Desktop notifications for the end of Pi's turns.

## Overview

The Notify Turn Complete extension alerts you when Pi finishes a turn. It does this by sending a native desktop notification directly through your terminal emulator using the standard OSC 9 escape sequence. This is especially useful for long-running agent tasks where you might switch away from the terminal.

### Supported Terminals

Terminals that support OSC 9 include:

- **Ghostty**
- **Kitty**
- **iTerm2**

When running inside **tmux**, the extension automatically wraps the escape sequence in a Device Control String (DCS) passthrough envelope so that tmux forwards it to the host terminal.

## Architecture

The extension is simple and lightweight. It consists of a single entry point that hooks into Pi's lifecycle.

```
notify-turn-complete/
├── index.ts   # Entry point: text extraction and escape-sequence writer
└── README.md             # This file
```

### Key Flows

**Emitting Notifications** (`agent_end` hook):

1. The `agent_end` lifecycle hook fires after the agent finishes its turn.
2. The event's message history is inspected to find the last assistant message.
3. The content of the last assistant message is extracted and normalized.
4. The message text is sanitized: newlines removed, truncated to 200 chars.
5. An OSC 9 escape sequence is generated containing the sanitized text.
6. If a tmux session is detected, the OSC 9 payload is wrapped in a DCS passthrough sequence.
7. The complete payload is written directly to stdout.

## User-Facing Surface

This extension runs completely in the background. It does not provide any:

- Slash commands (e.g., `/notify`)
- Settings or configuration flags
- UI status-bar elements

A native desktop notification pops up automatically after each agent turn is completed, showing a 200-character preview of Pi's response. If no assistant message text is found, it falls back to showing `"Pi has finished their turn"`.

## Files

| File       | Description                                     |
| :--------- | :---------------------------------------------- |
| `index.ts` | Complete implementation. Registers the hook and |
|            | writes escape codes.                            |

## Dependencies and API Integration

### Pi Agent API Calls

The extension registers a single lifecycle hook:

- `pi.on("agent_end", (event) => { ... })` - Emits the notification payload at the end of each turn.

### Node APIs Used

- `process.stdout.write(output)` - Directly writes raw escape codes to stdout.
- `process.env.TMUX` - Detects if the agent is running within a tmux session.

## Notable Implementation Details

- **Content Normalization:** Supports both simple string content and Anthropic/OpenAI-style arrays of content blocks in assistant messages. It filters and joins blocks where `type === "text"` to handle structured model outputs gracefully.
- **Payload Truncation:** To prevent overflowing terminal notification payloads or cluttering the system notification history, the message body is truncated to 200 characters.
- **Sanitization:** All newlines are replaced with spaces so that the notification is kept on a single line and displays nicely in desktop notifications.
- **Tmux Passthrough:** When `TMUX` is set in the environment, the payload is wrapped in a DCS passthrough envelope (`\x1bPtmux;... \x1b\\`) and ESC bytes are doubled (`\x1b` -> `\x1b\x1b`). For this to work, `allow-passthrough` must be enabled in your tmux configuration.
- **UI-Agnostic Execution:** The extension does not check `ctx.hasUI`. Therefore, it will emit notifications even in `--print` or JSON output modes, making sure you are notified whenever the agent's turn finishes.

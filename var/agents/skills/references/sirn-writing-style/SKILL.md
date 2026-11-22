---
name: sirn-writing-style
type: reference
description: Use when writing, rewriting, or reviewing documentation, READMEs, guides, API docs, ADRs, blog posts, technical notes, docstrings, or code and configuration comments, alongside simple-english, or when prose reads as choppy, staccato, or disconnected between sentences.
---

# Writing Style

Pair this skill with `simple-english` for repository and published technical writing (docs, READMEs, API references, guides, ADRs, notes, docstrings, and code comments). For conversational replies, forum threads, PR reviews, chat, and email, use `sirn-communication-style` instead. While `simple-english` governs active voice, simple tenses, plain words, no AI slop, no em-dashes, and the 25-word descriptive / 20-word procedural sentence cap, this skill governs how sentences join into cohesive paragraphs, how technical documents are structured, and how docstrings and comments record contracts and constraints.

## Audience & Definitions

- **Do not gloss standard engineering terms**: For developer docs, READMEs, ADRs, and technical notes, treat everyday computing terms (`TCP handshake`, `prepared statement`, `cgroup`, `DMA`, `JSON Lines`) as known. Define only project-specific concepts, domain-external terms, or non-obvious acronyms at first use.
- **Motivate rules with the underlying constraint**: State _why_ a default or recommendation exists before or alongside the rule (for example, why CFS quotas add tail latency even on idle hosts, or why scaling a cache on memory usage backfires).

## Paragraph Shape

- **Flowing paragraph lines**: Write all sentences of a paragraph on one standard Markdown line separated from other paragraphs by a blank line. Never use one-sentence-per-line semantic linebreaks in prose, because they encourage isolated bullet-like statements.
- **Complete 3–5 sentence arcs in prose**: Build each explanatory paragraph around one topic arc (baseline or mechanism, consequence or concrete example, and resolution or edge case). Avoid 1–2 sentence stub paragraphs in conceptual prose and avoid opening sections with tautological dictionary definitions (`Connection pooling reuses database connections.`). Keep setup step lead-ins, convention rules, API endpoint summaries, and docstrings concise (1–2 sentences) rather than padding them into multi-sentence paragraphs.

## Joining Sentences (Old-to-New Chaining)

Every sentence after the first must either start from a concept introduced at the end of the previous sentence or name the logical relation between the two:

| Relation | Opening pattern |
| --- | --- |
| **Anaphoric handoff** (pick up prior tail) | `This adds ...`, `That handshake ...`, `Inside that sandbox, ...`, `Under this policy, ...` |
| **Baseline vs. override** | `By default, <baseline>. To <goal>, <command>.` / `Instead, ...` |
| **Cause & consequence** | `Because <constraint>, <behavior>.` / `This cuts ...` / `As a result, ...` |
| **Concrete illustration** | `For example, if <concrete numbers>, <outcome>.` |
| **Contrast & boundary** | `In contrast, ...` / `By contrast, ...` / `Otherwise, ...` / `Unlike <X>, <Y> ...` |
| **Sequence & lifecycle** | `At startup, ...` -> `If ..., ...` -> `Once <step> succeeds, ...` |

- **Bridge every sentence split**: When splitting a >25-word compound sentence to stay within the `simple-english` word cap, replace `, while` or `, so` with a period _plus_ an explicit bridge (`A. In contrast, B.` or `A. This lets B.`), never a bare period (`A. B.`).
- **Bridge across paragraphs**: Open a new paragraph by anchoring it to the preceding paragraph (`To avoid those handshake spikes, PgBouncer...`, `Unlike hard compaction, [compaction.auto] waits...`, `Alongside text caps, [image] bounds...`).

### Before vs. After

Before (unjoined `simple-english` sentences on separate lines):

> Lofi spawns Claude Code in streaming print mode connected to `lofi mcp serve`. Built-in Claude Code tools, slash commands, hooks, and local Claude configuration are disabled for the child. Enterprise managed Claude policies still apply. The child `init` frame must report only the `exec` tool and only the Lofi MCP server. Any other reported tool or server aborts the turn with an error naming it.

After (topic-chained paragraph):

> Lofi spawns Claude Code in streaming print mode and connects it to `lofi mcp serve`. To keep tool calls inside Lofi, the child disables built-in Claude tools, slash commands, hooks, and local configuration, while enterprise policies still apply. At startup, the child must send an `init` frame within 30 seconds that lists only the `exec` tool and the Lofi MCP server. If that frame reports any other tool or server, Lofi aborts the turn with an error naming it.

## Document Shapes & Mechanics

- **READMEs and setup guides**: Open with one direct sentence stating what the project does (no hype or mission pitch), and state explicit non-goals or maturity upfront (such as a `Status` table for incomplete support). Separate runtime and build-time requirements, use aligned directory trees with concrete example mappings when explaining file layouts, walk top-down from entrypoint to one end-to-end example, and keep setup sequences command-first with short goal-led lead-ins ending in a colon (`Clone the repository:`, `Configure Nix in ~/.config/nix/nix.conf:`) directly above fenced blocks (`sh`, `toml`, `ini`, `nix`).
- **API and configuration references**: Lead each endpoint or section with 1–2 sentences stating what it does, its default behavior, and non-obvious async or semantic caveats. Present parameters, response fields, and env vars in a `Field | Type | Description` table (or compact `-` list) with explicit `Required` markers, defaults, mutual exclusions, conditional availability (`Only present when ...`), and a concrete example value per field. Call out semantic traps and anti-patterns directly beside the field or method (for example, when `status: "done"` indicates worker completion rather than domain success, or why calling a sync method on every request backfires).
- **Convention guides and articles**: Lead conceptual sections with a cohesive paragraph explaining the mechanism and trade-offs, then present multi-field details in a Markdown table or language-tagged code block (`sh`, `toml`, `json`, `go`, `rust`). For convention guides, state each rule concisely, follow immediately with a minimal concrete snippet, and record the underlying constraint beside the rule.
- **Working, ops, and debugging notes**: Use concise `-` bullet lists with indented sub-bullets under direct headings (`Symptom`, `Root cause`, `Fix`, `Result`, `Gotchas`), keeping cause-and-effect chains explicit.
- **Parentheses, arrows, and serial commas**: Use parentheses `(...)` for compact qualifiers, versions, and examples (`(e.g. ...)`, `(i.e. ...)`) rather than em-dashes (`—`), `->` for transitions or replacements, and the Oxford comma (`a, b, and c`) so list groupings are unambiguous.
- **Concrete numbers**: Ground latency, capacity, memory, and timeout explanations in concrete numbers and units whenever known (`60-100ns` vs `67.2ns` frame budget, `10,000,000 objects × 1 KB = 10 GB`).

## Docstrings & Code Comments

- **Terse docstrings by contract and contrast**: Open with a direct 1-sentence summary (`Returns True if ...`, `Works like X, except ...`) without boilerplate (`This method ...`) or repeating defaults already visible in the signature. State explicit boolean, fallback, and error contracts (`Returns False if not listed or no providers are configured`; `Raises ValueError when confidence is below 0.5`), and add parameter lines only when units, types, or semantics are non-obvious.
- **Comment constraints, not mechanics**: Record only non-obvious platform quirks, load or ordering dependencies, ABI or version mismatches, rejected alternatives, and candidly flagged hacks (`Hack: ...`). When documenting a non-obvious string or wire format, include one concrete example broken down by segment. Leave self-evident settings and straightforward code uncommented.
- **Cite primary sources inline**: Place issue URLs, PRs, manpages, or source references directly above workarounds and adapted code (`BUG: <issue-url>`, `Workaround for <issue-url>`, `See <flag> in <tool>(<section>)`, `Ref: <path> (<symbol>)`, or a pinned upstream commit URL).
- **Compress cause and effect**: Keep inline comments to 1–3 tight lines using `because`, `since`, `otherwise`, `;`, or `(...)` qualifiers (`# Non-Linux host; assume no systemd`, `# Load before TLS init; otherwise custom CAs are ignored`).
- **Plain labels, no decorative banners**: Start standalone scripts with a one-line purpose comment (`# Wrapper for running ...`) and group long configs with short 1–3 word section labels (`## Utilities`, `# Network rules`), without ASCII separator bars.

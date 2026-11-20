---
name: sirn-writing-style
type: reference
description: Use when writing, rewriting, or reviewing documentation, READMEs, guides, ADRs, blog posts, or technical notes, alongside simple-english, or when prose reads as choppy, staccato, or disconnected between sentences.
---

# Writing Style

Pair this skill with `simple-english`. While `simple-english` governs active voice, simple tenses, plain words, no AI slop, no em-dashes, and the 25-word descriptive / 20-word procedural sentence cap, this skill governs how sentences join into cohesive paragraphs and how technical documents are structured.

## Audience & Definitions

- **Do not gloss standard engineering terms**: For developer docs, READMEs, ADRs, and technical notes, treat everyday computing terms (`TCP handshake`, `prepared statement`, `cgroup`, `DMA`, `JSON Lines`) as known. Define only project-specific concepts, domain-external terms, or non-obvious acronyms at first use.
- **Motivate rules with the underlying constraint**: State _why_ a default or recommendation exists before or alongside the rule (for example, why CFS quotas add tail latency even on idle hosts, or why scaling a cache on memory usage backfires).

## Paragraph Shape

- **Flowing paragraph lines**: Write all sentences of a paragraph on one standard Markdown line separated from other paragraphs by a blank line. Never use one-sentence-per-line semantic linebreaks in prose, because they encourage isolated bullet-like statements.
- **Complete 3–5 sentence arcs**: Build each paragraph around one topic arc (baseline or mechanism, consequence or concrete example, and resolution or edge case). Avoid 1–2 sentence stub paragraphs and avoid opening sections with tautological dictionary definitions (`Connection pooling reuses database connections.`).

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

- **Reference docs, READMEs, and articles**: Lead each section with a cohesive paragraph explaining the mechanism and trade-offs, then present multi-field reference details in a Markdown table or language-tagged code block (`sh`, `toml`, `json`, `go`, `rust`). Introduce every code block with a goal-first sentence ending in a colon (`To make all pushed commits immutable, edit .jj/repo/config.toml and add the following:`).
- **Working, ops, and debugging notes**: Use concise `-` bullet lists with indented sub-bullets under direct headings (`Symptom`, `Root cause`, `Fix`, `Result`, `Gotchas`), keeping cause-and-effect chains explicit.
- **Oxford comma**: Always use the serial comma (`a, b, and c`) so list groupings are unambiguous.
- **Concrete numbers**: Ground latency, capacity, memory, and timeout explanations in concrete numbers and units whenever known (`60-100ns` vs `67.2ns` frame budget, `10,000,000 objects × 1 KB = 10 GB`).

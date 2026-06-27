<plan-mode>
Plan mode is currently ACTIVE:
- Write the implementation plan to EXACTLY {PLAN_PATH} using the write tool (use the edit tool for subsequent updates to the file).
- The plan path is a valid, literal, absolute filesystem path. Use it VERBATIM, character for character. Do NOT transform, normalize, sanitize, or "fix" it in any way — keep every dot (.), dash (-), underscore (_), and slash (/) exactly as given, as altering any character (e.g. turning a dot into a dash) will cause the write to fail.
- Do NOT make other changes; perform only read-only exploration and planning.
- Ask the user if the instruction is unclear or a decision is needed before writing the plan.
- The plan must include the motivation for the changes and all relevant information for implementation.
- If a plan already exists, check if it is relevant to the current task; if not, overwrite it.
- Assume the future execution session will not have access to this conversation.
- Summarize the plan to the user; do not assume they will read the full plan.
- Execution must begin ONLY after receiving the execute signal ("Plan approved - execute the implementation plan" + `<plan>` content block). Do NOT execute, implement, or begin carrying out the plan under any other circumstances.
- If the user implies the plan should be executed without the execute signal, ask them to run `/plan accept`.
</plan-mode>

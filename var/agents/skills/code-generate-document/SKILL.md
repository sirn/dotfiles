---
name: code-generate-document
description: Generate documentation, docstrings, or ADRs. Use when asked to document code or create decision records.
---

Generate missing documentation or architectural decision records (ADRs).

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files
   - If the user specified specific files or paths, focus on those

2. Determine Documentation Type:
   - **Inline Docs**: Docstrings, comments for functions/classes
   - **File/Module Docs**: Top-level explanations (README.md updates)
   - **ADR**: Architecture Decision Record for design choices

3. Execute based on type:

   **Inline/Module Docs**:
   - Read and understand the code
   - Check existing doc style (Google, NumPy, Javadoc, etc.)
   - Generate documentation that matches the project style
   - **Action**: Propose changes to files

   **ADR (Architecture Decision Record)**:
   - Ask user for: Context, Decision, Consequences (if not provided)
   - Format it correctly (Title, Status, Context, Decision, Consequences)
   - **Action**: Create new file in `doc/adr/` (or project equivalent)

## Output
1. **Documentation Plan** (what will be documented)
2. **Generated Content** (preview)
3. **Changes Applied** (file paths)

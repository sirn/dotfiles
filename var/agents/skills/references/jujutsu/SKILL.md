---
name: jujutsu
type: reference
description: Reference for Jujutsu (jj) version control commands. ALWAYS read before performing ANY jj operation (commit, squash, rebase, describe, split, etc.) to ensure correct syntax and best practices.
---

## Jujutsu Command Reference

Working copy is always a commit. Changes are first-class with stable IDs across rewrites.

### Best Practices

- **Local Commit Autonomy**: `jj describe`, `jj commit`, and `jj new` are allowed when they are part of the current requested task and stay local.
- **User Authorization**: NEVER push (`jj git push`) or run destructive/history-rewriting operations (`jj edit`, `jj squash` across bookmarks/shared history, `jj split`, `jj rebase`, `jj abandon`, `jj undo`, `jj op restore`, bookmark moves/deletes) without explicit user confirmation. Routine local amends (`jj squash`, `jj squash --into @-`) do not need confirmation.
- **Bookmark Creation**: NEVER run `jj bookmark create` unless the user explicitly asks for a bookmark. When the user does ask, create only the bookmark(s) they requested — do not create a new bookmark per commit unless the user explicitly asked for that.
- **Logical Commits**: Group changes into logical steps; try to make each commit "usable" on its own.
- **Commit + Advance**: Prefer `jj commit -m "msg"` when finalizing the current working-copy commit and moving on. This replaces the common `jj describe <id> -m "msg"` followed by `jj new <id>` sequence.
- **Working Copy After Commit**: After `jj commit` or `jj new`, a new empty commit becomes `@`. This is expected jj behavior — do not attempt to remove or squash it away.
- **Revision References**: Use `@`, `@-`, and revsets for immediate one-off commands when they are clear. Use explicit change IDs for scripts, multi-step instructions, destructive operations, and commands where the target could become ambiguous.
- **Splitting**: Use `jj split -r <change-id> -m "<commit-message>" -- <file>`; do not use interactive `jj split`.
- **Squashing**: Use `jj squash --from <from-id> --to <to-id>` instead of implicit `jj squash`. Prefer **from newer to older** (descendant to ancestor) to match default `jj squash` and minimize conflicts. Squashing older into newer (e.g., `--from <base> --to <head>`) rewrites head's ancestors, forcing jj to rebase descendants and resolve any overlapping diff conflicts.
- **Commit Messages**: Keep subject line <= 72 characters; body lines <= 72 characters. Use imperative mood ("add feature" not "added feature"). Explain "what" and "why", not "how".

### Key Concepts

- `@` = working copy commit
- `@-` = parent, `@--` = grandparent
- Revsets: `::@` (ancestors), `main..@` (commits since main)
- Default `jj log` shows only mutable revisions (plus working-copy + bookmarked context). Use `jj log -r ::` or `all()` to see all visible commits, including immutable ancestors.
- Immutable commits: those in `::immutable_heads()` (default includes `trunk()`-ancestors) are immutable; jj refuses to mutate them. Bypass with `--ignore-immutable` only when you intend to rewrite them.

### Day-to-Day Commands

| Task                         | Command                                                     |
| ---------------------------- | ----------------------------------------------------------- |
| Status                       | `jj status` (Repo status) / `jj show <id>` (Change summary) |
| Diff                         | `jj diff -r <id>`                                           |
| Log                          | `jj log -r <revset>`                                        |
| Finalize current + move on   | `jj commit -m "msg"`                                        |
| New working commit on parent | `jj new <parent-id> -m "msg"`                               |
| Describe without moving      | `jj describe <id> -m "msg"`                                 |
| Navigate                     | `jj edit <id>`                                              |
| Abandon                      | `jj abandon <id>`                                           |
| Squash                       | `jj squash --from <from-id> --to <target-id>`               |
| Split commit                 | `jj split -r <id> -m "msg" -- <path>`                       |
| Rebase                       | `jj rebase -r <id> -d <dest>`                               |
| Show file                    | `jj file show <path> -r <id>`                               |
| Blame                        | `jj file annotate <path> -r <id>`                           |
| Resolve                      | `jj resolve -r <id>`                                        |
| Undo                         | `jj undo`                                                   |

### Interactive Mode

Use `jj split` interactively to review changes visually or when split boundaries are unclear. To drive `jj split` programmatically from another agent, see the **tmux** skill.

**Launching interactive split:**

```bash
# Split a specific commit interactively
jj split -r <change-id>

# Split the working copy interactively
jj split

# Split with a starting message (still interactive for content selection)
jj split -m "Extract auth utilities"
```

**Interactive behavior:**

`jj split` without `-- <paths>` opens a **diff editor** (`ui.diff-editor`, override with `--tool <name>`). Edit the _right side_ to select what goes into the first commit; what remains stays in the second. If the original commit has a description, jj prompts for two new descriptions in `$EDITOR`.

**When to use interactive vs. non-interactive:**

| Scenario                     | Approach        | Command                               |
| ---------------------------- | --------------- | ------------------------------------- |
| Clear file boundaries        | Non-interactive | `jj split -r <id> -m "msg" -- <path>` |
| Need to review diff visually | Interactive     | `jj split -r <id>`                    |
| Mixed changes in single file | Interactive     | `jj split`                            |
| Automated scripts            | Non-interactive | `jj split -r <id> -m "msg" -- <path>` |

**After splitting:**

```bash
# Check the resulting commits
jj log -r "<original-id>::"

# Verify the split was correct
jj diff -r <new-commit-id>
```

### Limiting Output

| Goal                    | Option                   | Example                                |
| ----------------------- | ------------------------ | -------------------------------------- |
| Limit commit count      | `-n <N>` / `--limit <N>` | `jj log -r ::@ -n 10`                  |
| Summary only (diffs)    | `-s` / `--summary`       | `jj diff -s -r <id>`                   |
| No graph (cleaner log)  | `--no-graph`             | `jj log -r ::@ --no-graph`             |
| Custom template         | `-T <template>`          | `jj log -r @ -T 'description ++ "\n"'` |
| Limit description lines | `-T "..."`               | `jj log -T 'description.first_line()'` |

### Revset Syntax

```
# Operators
x-          # Parents
x+          # Children
::x         # Ancestors (inclusive)
x::         # Descendants
x..y        # y ancestors excluding x ancestors
x & y       # Intersection
x | y       # Union

# Functions
mine()                  # Your commits
bookmarks()             # All bookmarks
remote_bookmarks()      # Remote bookmarks
author("pattern")       # By author
description("text")     # By message
files("path/**")        # Touching files
empty()                 # Empty commits
heads(x)                # Heads in set
```

### Bookmarks (like git branches)

```bash
jj bookmark create <name> -r <id>    # Create
jj bookmark set <name> -r <id>       # Set/update
jj bookmark move <name> --to <id>    # Move existing
jj bookmark delete <name>            # Delete
jj bookmark track <name> --remote origin  # Track remote bookmark
```

### Working with Remotes

#### Figuring Out Remote Repository

To find the remote repository URL (for `gh -R owner/repo`):

```bash
# List all remotes with URLs
jj git remote list

# Example output:
# origin  git@github.com:owner/repo.git (fetch)
# origin  git@github.com:owner/repo.git (push)

# Parse owner/repo from remote URL
jj git remote list | grep origin | head -1 | sed -E 's/.*github\.com[:/]([^/]+)\/([^/]+)\.git.*/\1\/\2/'
```

Common patterns to extract `owner/repo`:

- SSH: `git@github.com:owner/repo.git` → `owner/repo`
- HTTPS: `https://github.com/owner/repo.git` → `owner/repo`

#### Remote Operations

```bash
jj git fetch                          # Fetch all
jj git push --bookmark feature        # Push bookmark (new ones auto-track on push)
jj git push --all                     # Push all bookmarks
```

#### Colocated Repos

In colocated repos (having a `.git` directory), jj auto-imports Git refs before commands and auto-exports bookmarks/commits to Git after mutations (no need to run `jj git export` manually). Drive these explicitly when needed:

```bash
jj git import          # Pull Git refs/changes into jj
jj git export          # Write jj bookmarks/commits back into Git
```

### Common Workflows

#### Squash workflow (recommended)

Always squash from newer (descendant) into older (ancestor) to minimize conflicts:

```bash
# Correct: newer into older — minimizes conflicts
jj squash --from @- --to @--
jj squash --from <child-id> --to <parent-id>

# Wrong: older into newer — rewrites ancestors; descendants with overlapping diffs conflict
jj squash --from <parent-id> --to <child-id>
```

Squashing older into newer rewrites the head's ancestors, forcing jj to rebase descendants. While jj preserves diffs, descendants with overlapping changes require re-merging, making newer-into-older the preferred direction.

#### Feature branch

```bash
# If changes are in the current working-copy commit, finalize it and move on
jj commit -m "feat: add feature"

# If starting from a specific parent before making changes
jj new <main-id> -m "feat: add feature"

jj bookmark create <name> -r <id>
jj git push --bookmark <name>     # New bookmarks auto-track on push
```

#### Resolve conflicts

Conflicts in jj are stored _in the commit_ rather than the working tree. They are flagged by `jj status` and `jj log` and persist across operations until resolved. jj materializes file conflicts as 3-way merge markers; resolve them by editing the markers manually. To surface conflicts between two heads, use `jj new @ <other>` to create a merge commit.

```bash
jj resolve --list -r <id>          # List conflicts
jj resolve -r <id>                 # Use merge tool
jj resolve --tool=:ours -r <id>    # Accept current
jj resolve --tool=:theirs -r <id>  # Accept incoming
```

#### Recovery

Four distinct recovery tools — pick by scope:

- **`jj restore`**: Restores _file contents_ from one revision to another (e.g., to undo working-copy edits or discard changes to specific paths). Does **not** rewrite history; only affects the destination revision.
- **`jj undo`**: Undoes the last _operation_ (commit, rebase, squash, etc.) by creating a new inverse operation. Repeated calls walk further back. See also `jj redo`.
- **`jj op restore <op-id>`**: Restores the entire repository to its state at a specific past operation. Use `jj op log` to find the operation ID and `jj --at-op=<op-id> log` to preview that state first.
- **`jj revert -r <id>`**: Creates _new_ commits applying the reverse of a revision's diff (no history rewrite). Distinct from `jj restore --changes-in` (in-place in the same commit) and `jj op revert` (operation-level).

```bash
# File-level recovery (no history rewrite)
jj restore                         # Discard working-copy changes (restore @ from @-)
jj restore <path>                   # Discard working-copy changes for specific paths only
jj restore --from <src-id> --to <dst-id>   # Copy file contents from src into dst
jj restore --changes-in <id>       # Undo the diff introduced by <id> vs its parents
jj revert -r <id> -d <dest>        # New commits reversing <id>'s diff onto <dest> (no rewrite)

# Operation-level recovery (rewrites history since the op)
jj undo                            # Undo the last operation; repeat to go further back
jj redo                            # Re-apply what a previous `jj undo` removed
jj op log                          # View operation history to find an op-id
jj --at-op=<op-id> log             # Preview repo state at an operation before restoring
jj op restore <op-id>             # Restore the whole repo to that operation's state
```

**When to use which:** Prefer `jj restore` for narrow, content-only changes (like unwanted working-copy edits). Use `jj undo` for the most recent mistake. Use `jj op restore` to jump back to an arbitrary earlier state. Since `jj undo` and `jj op restore` rewrite history, always confirm with the user first.

#### Pitfalls

- **`jj restore` without paths or `--from`/`--into`** restores _all_ files in the working copy from its parent, discarding all local changes (keeps the empty commit, similar to `jj abandon`). Always pass a `<path>` to scope it (e.g., `jj restore path/to/file`).
- **`jj restore` does not undo operations.** It only copies file contents. To reverse a rebase, squash, or abandon, use `jj undo` or `jj op restore`.
- **`jj undo` is non-selective and does not redo.** It reverts the entire last operation. Repeatedly running `jj undo` walks further back. Use `jj redo` to re-apply a prior undo.
- **`jj op restore <op-id>` restores the _whole repo_**, rolling everything since that operation into one new state. Always preview with `jj --at-op=<op-id> log` first, and prefer `jj restore` if you only want to revert a single revision's content.
- **`jj restore --changes-in <id>` on a merge** reverts the diff against the _merge of the parents_, which may not match either parent's content. Inspect with `jj diff -r <id>` first.

### Operation Log

The operation log is jj's main recovery surface — every mutating command is an op.

```bash
jj op log                              # Operation history
jj op show [op-id]                     # What an op changed (-p for patch)
jj op diff --op <op-id>                # Repo diff between an op and its parent
jj op revert <op-id>                   # New op applying the inverse of one op
jj op restore <op-id> [--what repo]    # Restore to a state; --what limits scope
jj op abandon <op-id>                  # Discard old op history (then `jj util gc`)
```

Use `jj --at-op=<op-id> <cmd>` with read-only commands (e.g., `log`, `show`, `diff`) to preview repo state at an operation. `jj evolog` shows how a single change (change ID) evolved across rewrites, complementing `jj op log`.

### Editing Revision Content

```bash
jj diffedit -r <id>                    # Interactively edit a revision's content (diff editor)
jj diffedit --from <a> --to <b>        # Edit the diff between two revisions
jj diffedit --tool <name> -r <id>      # Use a specific diff editor
```

Use `jj diffedit` for partial restores or interactive edits not covered by `jj restore` (whole files) or `jj squash -i` (moving changes).

### Configuration

```bash
jj config list [pattern]               # Show config values
jj config get <key>                    # Print one value
jj config set --user <key> <val>       # Write to user config
jj config path [--user|--repo]         # Print config file path
jj config edit [--user|--repo]         # Open config in $EDITOR
```

Common keys: `revset-aliases.immutable_heads()`, `git.push` (`auto`|`branch`|`current`), `ui.diff-editor` / `ui.merge-editor`, and `templates.*`.

### Ignoring Files

jj respects `.gitignore` (it has no `.jjignore`). `snapshot.auto-track` controls whether new files are tracked automatically (default on). Once tracked, a file remains tracked even if matched by a later ignore pattern.

```bash
jj file track <path>                   # Start tracking (when auto-track is off)
jj file untrack <path>                 # Stop tracking; file stays in working copy
jj file list -r <id>                   # List files in a revision
```

### Workspaces

Jujutsu supports workspaces (similar to `git worktree`) to work on multiple branches simultaneously.

#### Key Points

- **Workspace Isolation**: Workspaces allow concurrent development, but changes are not automatically reflected in the main repository.
- **Tooling Scope**: External commands (like `docker exec` or `podman exec`) typically operate on the main repository, not the active workspace.
- **Sync Requirement**: You must explicitly track the workspace from the main repository to import its changes.

#### Common Workspace Operations

```bash
# From the local source repository, track a workspace
jj edit <workspace-name>@

# Update stale workspaces
jj workspace update-stale
```

#### Working with Workspaces

When working in a workspace:

1. **Keep Changes Local**: Uncommitted changes remain local to the workspace.
2. **Sync to Source**: Navigate to the source repository and run workspace commands to synchronize state.
3. **Verify Paths**: Always check your working directory when running external builds or containers (like `docker exec` or `podman exec`).

```bash
# Example: sync workspace changes to source repo
cd /path/to/source/repo
jj edit my-workspace@
jj workspace update-stale
```

---
name: jujutsu
type: reference
description: Reference for Jujutsu (jj) version control commands. ALWAYS read before performing ANY jj operation (commit, squash, rebase, describe, split, etc.) to ensure correct syntax and best practices.
---

## Jujutsu Command Reference

Working copy is always a commit. Changes are first-class with stable IDs across rewrites.

### Best Practices

- **Local Commit Autonomy**: `jj describe`, `jj commit`, and `jj new` are allowed when they are part of the current requested task and stay local.
- **User Authorization**: NEVER push (`jj git push`) or run destructive/history-rewriting operations (`jj edit`, `jj squash`, `jj split`, `jj rebase`, `jj abandon`, `jj undo`, `jj op restore`, bookmark moves/deletes) without explicit user confirmation.
- **Logical Commits**: Group changes into logical steps; try to make each commit "usable" on its own.
- **Commit + Advance**: Prefer `jj commit -m "msg"` when finalizing the current working-copy commit and moving on. This replaces the common `jj describe <id> -m "msg"` followed by `jj new <id>` sequence.
- **Revision References**: Use `@`, `@-`, and revsets for immediate one-off commands when they are clear. Use explicit change IDs for scripts, multi-step instructions, destructive operations, and commands where the target could become ambiguous.
- **Splitting**: Use `jj split -r <change-id> -m "<commit-message>" -- <file>`; do not use interactive `jj split`.
- **Squashing**: Use `jj squash --from <from-id> --to <to-id>` instead of implicit `jj squash`. Always squash **from newer to older** (descendant into ancestor) to avoid conflicts. Squashing older into newer (e.g. `--from <base> --to <head>`) rewrites the head's ancestors while descendants still reference the old state, causing conflicts in every downstream commit.
- **Commit Messages**: Keep subject line <= 70 characters; body lines <= 70 characters. Use imperative mood ("add feature" not "added feature"). Explain "what" and "why", not "how".

### Key Concepts

- `@` = working copy commit
- `@-` = parent, `@--` = grandparent
- Revsets: `::@` (ancestors), `main..@` (commits since main)

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

Use `jj split` interactively when you need to review changes visually or when split boundaries aren't clear in advance. To drive `jj split` programmatically from another agent, see the **tmux** skill.

**Launching interactive split:**

```bash
# Split a specific commit interactively
jj split -r <change-id>

# Split the working copy interactively
jj split

# Split with a starting message (still interactive for file selection)
jj split -m "Extract auth utilities"
```

**Interactive behavior:**

When run without `-- <paths>`, `jj split` enters interactive mode where you can:

- Review each changed file interactively
- Choose which changes go into the first commit vs. second
- Edit commit messages in your configured editor
- View diffs before confirming

**What you'll see:**

| Prompt                       | Meaning                      | Response                  |
| ---------------------------- | ---------------------------- | ------------------------- |
| `Include this change? [y/n]` | Include file in first commit | `y` (yes) or `n` (no)     |
| `(END)` in pager             | Diff viewer (less) is open   | `q` to quit               |
| Editor opens                 | Writing commit message       | Write message, save, quit |
| `Continue? [y/n]`            | Proceed with second commit   | `y` to continue           |

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
| Summary only (status)   | `-s`                     | `jj status -s`                         |
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
jj bookmark track <name>@origin      # Track remote
```

### Working with Remotes

#### Figuring Out Remote Repository

To determine the remote repository URL (useful for `gh -R owner/repo` commands):

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
jj git push --bookmark feature        # Push bookmark
jj git push --bookmark new --allow-new  # Push new bookmark
```

### Common Workflows

#### Squash workflow (recommended)

Always squash from newer (descendant) into older (ancestor) to avoid conflicts:

```bash
# Correct: newer into older — no conflicts
jj squash --from @- --to @--
jj squash --from <child-id> --to <parent-id>

# Wrong: older into newer — causes conflicts on all descendants
jj squash --from <parent-id> --to <child-id>
```

When squashing older into newer, jj rewrites the target's parent commit.
All descendant commits still reference the old parent, so every file that
differs between old and new parent becomes a conflict.

#### Feature branch

```bash
# If changes are in the current working-copy commit, finalize it and move on
jj commit -m "feat: add feature"

# If starting from a specific parent before making changes
jj new <main-id> -m "feat: add feature"

jj bookmark create <name> -r <id>
jj git push --bookmark <name> --allow-new
```

#### Resolve conflicts

```bash
jj resolve --list -r <id>          # List conflicts
jj resolve -r <id>                 # Use merge tool
jj resolve --tool=:ours -r <id>    # Accept current
jj resolve --tool=:theirs -r <id>  # Accept incoming
```

#### Recovery

```bash
jj undo                     # Undo last operation
jj op log                   # View operation history
jj op restore <op-id>       # Restore to state
```

### Workspaces

Jujutsu supports workspaces (similar to `git worktree`) for working on multiple branches simultaneously.

#### Key Points

- When in a workspace, changes are **not automatically reflected** in the local source repository
- Some commands (e.g., `docker exec`, `podman exec`) may only point to the local source repository
- You may need to track the workspace explicitly from the source repository

#### Common Workspace Operations

```bash
# From the local source repository, track a workspace
jj edit <workspace-name>@

# Update stale workspaces
jj workspace update-stale
```

#### Working with Workspaces

When working in a workspace:

1. **Changes made in the workspace** stay in the workspace until committed
2. **To sync changes** to the source repo, navigate to the source and track the workspace
3. **External tools** (like Docker/Podman exec) may operate on the source repo, not the workspace

```bash
# Example: sync workspace changes to source repo
cd /path/to/source/repo
jj edit my-workspace@
jj workspace update-stale
```

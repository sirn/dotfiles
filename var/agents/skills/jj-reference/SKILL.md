---
name: jj-reference
type: reference
description: Reference for Jujutsu (jj) version control commands
---

## Jujutsu Command Reference

Working copy is always a commit. Changes are first-class with stable IDs across rewrites.

### Best Practices
- **Logical Commits**: Group changes into logical steps; try to make each commit "usable" on its own.
- **Explicit Operations**: Always use explicit change IDs for all operations (e.g., `abandon`, `describe`, `edit`, `new`, `squash`, `split`, `rebase`, `bookmark`).
- **Splitting**: Use `jj split -r <change-id> -m "<commit-message>" -- <file>`; do not use interactive `jj split`.
- **Squashing**: Use `jj squash --from <from-id> --to <to-id>` instead of implicit `jj squash`.
- **Direct Referencing**: Reference change-id directly (or unique prefix) instead of using `@-` or `@` to avoid ambiguity.

### Key Concepts
- `@` = working copy commit
- `@-` = parent, `@--` = grandparent
- Revsets: `::@` (ancestors), `main..@` (commits since main)

### Day-to-Day Commands

| Task | Command |
|------|---------|
| Status | `jj status` (Repo status) / `jj show <id>` (Change summary) |
| Diff | `jj diff -r <id>` |
| Log | `jj log -r <revset>` |
| New commit | `jj new <parent-id> -m "msg"` |
| Describe | `jj describe <id> -m "msg"` |
| Commit + new | `jj commit -m "msg"` |
| Navigate | `jj edit <id>` |
| Abandon | `jj abandon <id>` |
| Squash | `jj squash --from <from-id> --to <target-id>` |
| Split commit | `jj split -r <id> -m "msg" -- <path>` |
| Rebase | `jj rebase -r <id> -d <dest>` |
| Show file | `jj file show <path> -r <id>` |
| Blame | `jj file annotate <path> -r <id>` |
| Resolve | `jj resolve -r <id>` |
| Undo | `jj undo` |

### Limiting Output

| Goal | Option | Example |
|------|--------|---------|
| Limit commit count | `-n <N>` / `--limit <N>` | `jj log -r ::@ -n 10` |
| Summary only (diffs) | `-s` / `--summary` | `jj diff -s -r <id>` |
| Summary only (status) | `-s` | `jj status -s` |
| No graph (cleaner log) | `--no-graph` | `jj log -r ::@ --no-graph` |
| Custom template | `-T <template>` | `jj log -r @ -T 'description ++ "\n"'` |
| Limit description lines | `-T "..."` | `jj log -T 'description.first_line()'` |

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
```bash
jj new <parent-id>
# ... make changes ...
jj squash --from <change-id> --to <target-id>
```

#### Feature branch
```bash
jj new <main-id>
jj describe <id> -m "feat: add feature"
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

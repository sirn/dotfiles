---
name: jujutsu-reference
description: Reference for Jujutsu (jj) version control commands
---

## Jujutsu Command Reference

Working copy is always a commit. Changes are first-class with stable IDs across rewrites.

### Key Concepts
- `@` = working copy commit
- `@-` = parent, `@--` = grandparent
- Revsets: `::@` (ancestors), `main..@` (commits since main)

### Day-to-Day Commands

| Task | Command |
|------|---------|
| Status | `jj status` |
| Diff | `jj diff` / `jj diff -s` |
| Log | `jj log` / `jj log -r <revset>` |
| New commit | `jj new` / `jj new -m "msg"` |
| Describe | `jj describe -m "msg"` |
| Commit + new | `jj commit -m "msg"` |
| Navigate | `jj prev` / `jj next` |
| Edit commit | `jj edit <id>` |
| Squash to parent | `jj squash` |
| Split commit | `jj split` |
| Rebase | `jj rebase -d <dest>` |
| Abandon | `jj abandon` |
| Show file | `jj file show <path> -r <rev>` |
| Blame | `jj file annotate <path>` |
| Undo | `jj undo` |

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
jj bookmark create feature -r @    # Create
jj bookmark set feature -r @       # Set/update
jj bookmark move feature -r @      # Move existing
jj bookmark delete feature         # Delete
jj bookmark track feature@origin   # Track remote
```

### Working with Remotes

```bash
jj git fetch                          # Fetch all
jj git push --bookmark feature        # Push bookmark
jj git push --bookmark new --allow-new  # Push new bookmark
```

### Common Workflows

#### Squash workflow (recommended)
```bash
jj new                 # Start new commit
# ... make changes ...
jj squash              # Merge into parent
```

#### Feature branch
```bash
jj new main
jj commit -m "feat: add feature"
jj bookmark create my-feature -r @-
jj git push --bookmark my-feature --allow-new
```

#### Resolve conflicts
```bash
jj resolve --list           # List conflicts
jj resolve                  # Use merge tool
jj resolve --tool=:ours     # Accept current
jj resolve --tool=:theirs   # Accept incoming
```

#### Recovery
```bash
jj undo                     # Undo last operation
jj op log                   # View operation history
jj op restore <op-id>       # Restore to state
```

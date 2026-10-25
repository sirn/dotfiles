---
name: forgejo-cli
type: reference
description: Reference for Forgejo CLI (fj) - READ-ONLY operations only. ALWAYS read BEFORE using fj commands to ensure correct syntax and available flags.
---

## Forgejo CLI Reference (fj)

**IMPORTANT**: This skill is for READ-ONLY operations only. DO NOT execute any commands that create, modify, or delete resources.

The `forgejo-cli` package provides the `fj` binary.

### Repository Identification

By default, `fj` operates on the repository detected from the current directory.

| Method                 | Command                      |
| ---------------------- | ---------------------------- |
| Use `-R <remote>`      | `fj <command> -R origin`     |
| Use `-r/--repo <repo>` | `fj <command> -r owner/repo` |
| Use `-H <host>`        | `fj <command> -H <host>`     |

`-R/--remote` selects a **local git remote** by name (e.g. `origin`; for `pr`, `org`, and `user`, pass `-R` before the subcommand, e.g. `fj pr -R origin view <number>`; `fj whoami` uses `-r/--remote`). `-r/--repo` takes an `owner/repo` and is accepted by `release`, `tag`, `issue search`, `issue templates`, `pr search`, and `actions` subcommands. `-H/--host` overrides the host.

### Current User (Read-Only)

| Task   | Command     |
| ------ | ----------- |
| Whoami | `fj whoami` |

### Issues (Read-Only)

| Task                | Command                        |
| ------------------- | ------------------------------ |
| Search issues       | `fj issue search [query]`      |
| View issue          | `fj issue view <id>`           |
| View issue comments | `fj issue view <id> comments`  |
| View assignees      | `fj issue view <id> assignees` |
| List templates      | `fj issue templates`           |

**Search options**: `--state open|closed|all`, `--creator <user>`, `--assignee <user>`, `--labels <labels>`, `--mentions <user>`, `--type issues|pulls`

### Pull Requests (Read-Only)

| Task             | Command                         |
| ---------------- | ------------------------------- |
| Search PRs       | `fj pr search [query]`          |
| View PR          | `fj pr view <number>`           |
| View PR diff     | `fj pr view <number> diff`      |
| View PR files    | `fj pr view <number> files`     |
| View PR commits  | `fj pr view <number> commits`   |
| View PR comments | `fj pr view <number> comments`  |
| View PR labels   | `fj pr view <number> labels`    |
| View assignees   | `fj pr view <number> assignees` |
| PR status        | `fj pr status <number>`         |

**Search options**: `--state open|closed|all`, `--creator <user>`, `--assignee <user>`, `--labels <labels>`, `--mentions <user>`

### Repositories (Read-Only)

| Task           | Command                       |
| -------------- | ----------------------------- |
| View repo info | `fj repo view [repo]`         |
| View README    | `fj repo readme [repo]`       |
| View labels    | `fj repo labels [repo] view`  |
| Star status    | `fj repo star-status [repo]`  |
| Watch status   | `fj repo watch-status [repo]` |

### Releases (Read-Only)

| Task          | Command                 |
| ------------- | ----------------------- |
| List releases | `fj release list`       |
| View release  | `fj release view <tag>` |

### Tags (Read-Only)

| Task      | Command             |
| --------- | ------------------- |
| List tags | `fj tag list`       |
| View tag  | `fj tag view <tag>` |

### Organizations (Read-Only)

| Task         | Command                 |
| ------------ | ----------------------- |
| List orgs    | `fj org list`           |
| View org     | `fj org view <name>`    |
| List members | `fj org members <name>` |

### Users (Read-Only)

| Task | Command |
| --- | --- |
| Search users | `fj user search <q>` |
| View user | `fj user view <name>` |
| User repos | `fj user repos <name>` |
| User orgs | `fj user orgs <name>` |
| Followers/Following | `fj user followers <name>` / `fj user following <name>` |

### Actions (Read-Only)

| Task           | Command                     |
| -------------- | --------------------------- |
| List tasks     | `fj actions tasks`          |
| List secrets   | `fj actions secrets list`   |
| List variables | `fj actions variables list` |

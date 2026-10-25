---
name: github-cli
type: reference
description: Reference for GitHub CLI (gh). ALWAYS read BEFORE using gh commands to ensure correct syntax and available flags.
---

## GitHub CLI Reference (gh)

**IMPORTANT**: Read operations run without prompting. Write operations (creating, editing, commenting, closing, or merging issues/PRs/releases, or re-running/canceling Actions) require explicit user request and confirmation. Never run `gh repo delete`.

### Repository Identification

**Always be specific about which repository to use.**

| Method        | Command                      |
| ------------- | ---------------------------- |
| Use `-R` flag | `gh <command> -R owner/repo` |
| List remotes  | `jj git remote list`         |

**Best Practice**: Use `-R owner/repo` explicitly instead of relying on the default repository to avoid ambiguity.

### Issues (Read-Only)

| Task                | Command                                           |
| ------------------- | ------------------------------------------------- |
| List issues         | `gh issue list -R owner/repo`                     |
| View issue          | `gh issue view <number> -R owner/repo`            |
| View issue comments | `gh issue view <number> -R owner/repo --comments` |
| View issue (web)    | `gh issue view <number> -R owner/repo --web`      |
| Issue status        | `gh issue status -R owner/repo`                   |

**List options**: `--state open|closed|all`, `--label bug`, `--assignee @me`, `--limit 10`

### Pull Requests (Read-Only)

| Task                 | Command                                          |
| -------------------- | ------------------------------------------------ |
| List PRs             | `gh pr list -R owner/repo`                       |
| View PR              | `gh pr view <number> -R owner/repo`              |
| View PR comments     | `gh pr view <number> -R owner/repo --comments`   |
| View PR diff         | `gh pr diff <number> -R owner/repo`              |
| View PR checks       | `gh pr checks <number> -R owner/repo`            |
| View required checks | `gh pr checks <number> -R owner/repo --required` |
| PR status            | `gh pr status -R owner/repo`                     |

**List options**: `--state open|closed|merged|all`, `--label enhancement`, `--draft`, `--limit 10`

### Repository (Read-Only)

| Task            | Command                         |
| --------------- | ------------------------------- |
| View repo info  | `gh repo view owner/repo`       |
| View repo (web) | `gh repo view owner/repo --web` |
| List your repos | `gh repo list`                  |
| List org repos  | `gh repo list <org>`            |

### GitHub Actions - Workflow Runs (Read-Only)

| Task              | Command                                           |
| ----------------- | ------------------------------------------------- |
| List runs         | `gh run list -R owner/repo`                       |
| View run          | `gh run view <run-id> -R owner/repo`              |
| View failed logs  | `gh run view <run-id> -R owner/repo --log-failed` |
| View full run log | `gh run view <run-id> -R owner/repo --log`        |
| View specific job | `gh run view -R owner/repo --job <job-id>`        |
| Watch run         | `gh run watch <run-id> -R owner/repo`             |

**List options**: `--workflow <name>`, `--branch <branch>`, `--limit 10`, `--status completed|failure|in_progress`

### GitHub Actions - Workflows (Read-Only)

| Task               | Command                                            |
| ------------------ | -------------------------------------------------- |
| List workflows     | `gh workflow list -R owner/repo`                   |
| View workflow      | `gh workflow view <name\|id> -R owner/repo`        |
| View workflow YAML | `gh workflow view <name\|id> -R owner/repo --yaml` |

### GitHub Actions - Cache (Read-Only)

| Task        | Command                       |
| ----------- | ----------------------------- |
| List caches | `gh cache list -R owner/repo` |

**List options**: `--limit 10`, `--sort size_in_bytes|created_at|last_accessed_at`, `--order asc|desc`

### Releases (Read-Only)

| Task          | Command                               |
| ------------- | ------------------------------------- |
| List releases | `gh release list -R owner/repo`       |
| View release  | `gh release view <tag> -R owner/repo` |
| View latest   | `gh release view -R owner/repo`       |

### Search (Read-Only)

| Task           | Command                     |
| -------------- | --------------------------- |
| Search repos   | `gh search repos <query>`   |
| Search issues  | `gh search issues <query>`  |
| Search PRs     | `gh search prs <query>`     |
| Search code    | `gh search code <query>`    |
| Search commits | `gh search commits <query>` |

**Examples**:

```bash
gh search repos "language:go stars:>1000"
gh search issues "is:open label:bug repo:cli/cli"
gh search code "function main" --language=go
```

### Status (Read-Only)

| Task          | Command                                   |
| ------------- | ----------------------------------------- |
| View status   | `gh status`                               |
| Org status    | `gh status -o <org>`                      |
| Exclude repos | `gh status -e owner/repo1 -e owner/repo2` |

Shows assigned issues, PRs, review requests, mentions across your subscribed repositories.

### API (Read-Only GET Requests)

| Task | Command |
| --- | --- |
| GET endpoint | `gh api -X GET repos/owner/repo/<endpoint>` |
| With jq filter | `gh api -X GET repos/owner/repo/<endpoint> --jq '.[]'` |
| Paginate | `gh api -X GET repos/owner/repo/<endpoint> --paginate` |
| PR inline code comments | `gh api -X GET repos/owner/repo/pulls/<number>/comments` |
| PR review summaries | `gh api -X GET repos/owner/repo/pulls/<number>/reviews` |
| PR/issue comments | `gh api -X GET repos/owner/repo/issues/<number>/comments` |

**Examples**:

```bash
gh api -X GET repos/owner/repo/issues --paginate
gh api -X GET repos/owner/repo/pulls -q '.[] | {number, title}'
gh api -X GET repos/owner/repo/pulls/123/comments | jaq '.[] | {id, path, line, body}'
gh api graphql -f query='query { viewer { login } }'
```

**IMPORTANT**: Always pass `-X GET` on REST endpoints for read-only queries (note that passing `-f` or `-F` without `-X GET` defaults `gh api` to `POST` and triggers a confirmation prompt). Only execute `POST`, `PUT`, `PATCH`, or `DELETE` requests or GraphQL `mutation` queries when explicitly requested by the user.

### Write Operations (Require User Confirmation)

Always pass explicit `-t` / `-b` flags so `gh` does not open an interactive prompt or `$EDITOR`:

| Task | Command |
| --- | --- |
| Create issue | `gh issue create -R owner/repo -t "<title>" -b "<body>"` |
| Comment on issue | `gh issue comment <number> -R owner/repo -b "<body>"` |
| Edit issue | `gh issue edit <number> -R owner/repo -t "<title>" -b "<body>"` |
| Close issue | `gh issue close <number> -R owner/repo` |
| Create PR | `gh pr create -R owner/repo -B <base> -H <head> -t "<title>" -b "<body>"` |
| Comment on PR | `gh pr comment <number> -R owner/repo -b "<body>"` |
| Rerun failed CI | `gh run rerun <run-id> -R owner/repo --failed` |
| Delete cache | `gh cache delete <cache-id-or-key> -R owner/repo` |

### Common Output Options

| Goal | Flag | Example |
| --- | --- | --- |
| JSON output | `--json` | `gh issue list -R owner/repo --json number,title,state` |
| JSON fields | `--json <fields>` | `gh pr view 123 -R owner/repo --json headRefName,baseRefName` |
| Quiet (IDs only) | `--jq '.[] \| .number'` | `gh issue list -R owner/repo --jq '.[] \| .number'` |
| Web browser | `--web` | `gh issue view 123 -R owner/repo --web` |

### Identifying the Current Repository

Before running commands, determine the repository:

```bash
# From jj
jj git remote list
```

**Always use explicit `-R owner/repo`** to ensure you're querying the correct repository.

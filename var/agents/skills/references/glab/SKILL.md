---
name: glab
type: reference
description: Reference for GitLab CLI (glab). ALWAYS read BEFORE using glab commands to ensure correct syntax and available flags.
---

## GitLab CLI Reference (glab)

**IMPORTANT**: Read operations run without prompting. Write operations (creating, updating, commenting, closing, or merging issues/MRs/releases, or triggering/retrying CI pipelines) require explicit user request and confirmation. Never run `glab repo delete`.

### Repository Identification

**Always be specific about which repository to use.**

| Method        | Command                            |
| ------------- | ---------------------------------- |
| Use `-R` flag | `glab <command> -R owner/repo`     |
| Use `--repo`  | `glab <command> --repo owner/repo` |

**Best Practice**: Use `-R owner/repo` explicitly instead of relying on the default repository to avoid ambiguity. The value can be `OWNER/REPO`, `GROUP/NAMESPACE/REPO`, a full URL, or a Git URL.

### Issues (Read-Only)

| Task                | Command                                             |
| ------------------- | --------------------------------------------------- |
| List issues         | `glab issue list -R owner/repo`                     |
| View issue          | `glab issue view <number> -R owner/repo`            |
| View issue comments | `glab issue view <number> -R owner/repo --comments` |

**List options**: `--all`, `--closed`, `--assignee <user>`, `--author <user>`, `--label <label>`, `--milestone <n>`, `--search <text>`, `--per-page <n>`, `--output json`

### Merge Requests (Read-Only)

| Task                    | Command                                            |
| ----------------------- | -------------------------------------------------- |
| List MRs                | `glab mr list -R owner/repo`                       |
| View MR                 | `glab mr view <number> -R owner/repo`              |
| View MR comments        | `glab mr view <number> -R owner/repo --comments`   |
| View unresolved threads | `glab mr view <number> -R owner/repo --unresolved` |
| View MR diff            | `glab mr diff <number> -R owner/repo`              |

**List options**: `--all`, `--closed`, `--merged`, `--draft`, `--assignee <user>`, `--author <user>`, `--reviewer <user>`, `--label <label>`, `--milestone <n>`, `--search <text>`, `--per-page <n>`, `--output json`

### Repositories (Read-Only)

| Task            | Command                     |
| --------------- | --------------------------- |
| View repo info  | `glab repo view owner/repo` |
| List your repos | `glab repo list`            |
| List org repos  | `glab repo list -g <group>` |

### Releases (Read-Only)

| Task          | Command                                 |
| ------------- | --------------------------------------- |
| List releases | `glab release list -R owner/repo`       |
| View release  | `glab release view <tag> -R owner/repo` |

### CI/CD (Read-Only)

| Task              | Command                                   |
| ----------------- | ----------------------------------------- |
| List pipelines    | `glab ci list -R owner/repo`              |
| View pipeline     | `glab ci view [branch/tag] -R owner/repo` |
| Pipeline status   | `glab ci status -R owner/repo`            |
| Get pipeline JSON | `glab ci get -R owner/repo`               |
| Lint CI config    | `glab ci lint -R owner/repo`              |
| View job log      | `glab ci trace <job-id> -R owner/repo`    |

### Labels (Read-Only)

| Task        | Command                             |
| ----------- | ----------------------------------- |
| List labels | `glab label list -R owner/repo`     |
| Get label   | `glab label get <id> -R owner/repo` |

### Milestones (Read-Only)

| Task            | Command                                 |
| --------------- | --------------------------------------- |
| List milestones | `glab milestone list -R owner/repo`     |
| Get milestone   | `glab milestone get <id> -R owner/repo` |

### Schedules (Read-Only)

| Task           | Command                            |
| -------------- | ---------------------------------- |
| List schedules | `glab schedule list -R owner/repo` |

### Search (Read-Only)

| Task        | Command                                           |
| ----------- | ------------------------------------------------- |
| Search code | `glab search semantic -q "<query>" -R owner/repo` |

### API (Read-Only)

| Task | Command |
| --- | --- |
| GET endpoint | `glab api -X GET projects/owner%2Frepo/<endpoint>` |
| Paginate | `glab api -X GET --paginate projects/owner%2Frepo/<endpoint>` |
| MR discussions | `glab api -X GET projects/owner%2Frepo/merge_requests/<iid>/discussions` |
| MR notes | `glab api -X GET projects/owner%2Frepo/merge_requests/<iid>/notes` |

**IMPORTANT**: Always pass `-X GET` on read-only `glab api` requests. Passing `POST`/`PUT`/`PATCH`/`DELETE` methods (`-X`, `--method`) or data flags (`-F`/`--field`, `-f`/`--raw-field`, `--form`, `--input`) performs a write and requires explicit user request and confirmation.

### Write Operations (Require User Confirmation)

Always pass explicit `-t`, `-d`, `-m`, and `-y` (`--yes`) flags so `glab` does not open an interactive prompt or `$EDITOR`:

| Task | Command |
| --- | --- |
| Create issue | `glab issue create -R owner/repo -t "<title>" -d "<body>" -y` |
| Comment on issue | `glab issue note <number> -R owner/repo -m "<body>"` |
| Update issue | `glab issue update <number> -R owner/repo -t "<title>" -d "<body>"` |
| Close issue | `glab issue close <number> -R owner/repo` |
| Create MR | `glab mr create -R owner/repo -b <target> -s <source> -t "<title>" -d "<body>" -y` |
| Comment on MR | `glab mr note <number> -R owner/repo -m "<body>"` |
| Retry CI job | `glab ci retry <job-id> -R owner/repo` |

### Common Output Options

| Goal | Flag | Example |
| --- | --- | --- |
| JSON output | `--output json` | `glab mr list -R owner/repo --output json \| jaq .` |
| Pagination | `--page <n> --per-page <n>` | `glab issue list -R owner/repo --page 1 --per-page 50` |

**Note**: Prefer the long flag `--output json` over `-F json` or `-O json` (`glab issue list` binds `-O` to `--output` and `-F` to `--output-format`, whereas other subcommands bind `-F` to `--output`).

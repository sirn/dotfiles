---
name: asana
description: Interact with Asana API for task management, projects, and workspace operations using ASANA_PAT environment variable.
---

Asana API v1.0 reference for common operations.

## Prerequisites

- `ASANA_PAT` environment variable with a valid Asana Personal Access Token

**Check before use:**
```bash
[ -z "$ASANA_PAT" ] && echo "Error: ASANA_PAT not set" || echo "OK: ASANA_PAT is set"
```

**Note:** Never hardcode `ASANA_PAT`. Always use the environment variable.

## Base URL

```
https://app.asana.com/api/1.0
```

## Authentication

All requests require the `Authorization` header with Bearer token:

```bash
curl -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/..."
```

## Workspaces

### List Workspaces

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/workspaces" | jq '.data'
```

**Response**:
```json
[
  {
    "gid": "1234567890123456",
    "name": "My Workspace",
    "resource_type": "workspace"
  }
]
```

## Users

### Get Current User

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/users/me" | jq '.data'
```

### List Users in Workspace

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/workspaces/WORKSPACE_GID/users" | jq '.data'
```

## Projects

### List Projects

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/workspaces/WORKSPACE_GID/projects?archived=false" | jq '.data'
```

**Query Parameters**:
| Parameter | Description |
|-----------|-------------|
| `archived` | Include archived projects (`true`/`false`) |
| `team` | Filter by team GID |

### Get Project

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/projects/PROJECT_GID" | jq '.data'
```

**Response**:
```json
{
  "gid": "1234567890123456",
  "name": "Project Name",
  "notes": "Project description...",
  "color": "light-green",
  "archived": false,
  "resource_type": "project",
  "workspace": { "gid": "...", "name": "..." },
  "team": { "gid": "...", "name": "..." },
  "permalink_url": "https://app.asana.com/0/..."
}
```

## Sections (Columns/Board)

### List Sections in Project

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/projects/PROJECT_GID/sections" | jq '.data'
```

## Tasks

### Get Task

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/tasks/TASK_GID" | jq '.data'
```

**Response**:
```json
{
  "gid": "1234567890123456",
  "name": "Task Name",
  "notes": "Task description...",
  "assignee": { "gid": "...", "name": "..." },
  "assignee_status": "inbox",
  "completed": false,
  "due_on": "2025-03-15",
  "due_at": null,
  "tags": [{ "gid": "...", "name": "..." }],
  "projects": [{ "gid": "...", "name": "..." }],
  "memberships": [{ "section": { "gid": "...", "name": "..." } }],
  "permalink_url": "https://app.asana.com/1/...",
  "resource_type": "task"
}
```

### List Tasks

```bash
# Tasks in a project
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/projects/PROJECT_GID/tasks?completed_since=2025-01-01T00:00:00.000Z" | jq '.data'

# Tasks in a section
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/sections/SECTION_GID/tasks" | jq '.data'

# Tasks assigned to user
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/workspaces/WORKSPACE_GID/tasks/search?assignee=me&completed=false" | jq '.data'
```

**Query Parameters**:
| Parameter | Description |
|-----------|-------------|
| `completed_since` | Only tasks completed since timestamp |
| `modified_since` | Only tasks modified since timestamp |
| `limit` | Max results (default 20, max 100) |
| `offset` | Pagination offset |
| `opt_fields` | Specific fields to return |

### Search Tasks

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/workspaces/WORKSPACE_GID/tasks/search?text=KEYWORD&completed=false" | jq '.data'
```

**Search Parameters**:
| Parameter | Description |
|-----------|-------------|
| `text` | Text search in task name/description |
| `assignee` | Filter by assignee GID or `me` |
| `project` | Filter by project GID |
| `section` | Filter by section GID |
| `tag` | Filter by tag GID |
| `completed` | Filter by completion status |
| `due_on.before` | Due before date (YYYY-MM-DD) |
| `due_on.after` | Due after date (YYYY-MM-DD) |
| `modified_at.before` | Modified before timestamp |
| `modified_at.after` | Modified after timestamp |

## Stories (Comments)

### Get Task Stories (Comments)

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/tasks/TASK_GID/stories" | jq '.data'
```

## Tags

### List Tags

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/workspaces/WORKSPACE_GID/tags" | jq '.data'
```

### Get Tasks by Tag

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/tags/TAG_GID/tasks" | jq '.data'
```

## Common ID Patterns

- Task GIDs are numeric strings (e.g., "1201234567890123")
- Project GIDs follow same format
- All resources have a `gid` (globally unique identifier) field
- URLs contain GIDs: `https://app.asana.com/0/PROJECT_GID/TASK_GID`

## Utility: Extract IDs from URL

```bash
# Extract task GID from Asana URL
# From https://app.asana.com/0/123456789/9876543210
# Task GID is: 9876543210

echo "https://app.asana.com/0/123456789/9876543210" | sed 's/.*\///'
# Output: 9876543210

# Extract project GID (second-to-last segment)
echo "https://app.asana.com/0/123456789/9876543210" | awk -F'/' '{print $(NF-1)}'
# Output: 123456789
```

## Error Handling

| Status | Meaning |
|--------|---------|
| `200` | Success |
| `400` | Bad request (invalid parameters) |
| `401` | Unauthorized (invalid token) |
| `403` | Forbidden (insufficient permissions) |
| `404` | Not found |
| `429` | Rate limit exceeded (1500 req/min) |

## Best Practices

1. **Default to incomplete tasks**: Use `completed=false` unless asked for completed ones
2. **Use search for unknown IDs**: Start with workspace search to find tasks
3. **Use `opt_fields` for efficiency**: Request only needed fields to reduce payload size
4. **Handle pagination**: Use `offset` parameter for large result sets
5. **Comments are read-only**: Only read stories, do not create or update via this skill

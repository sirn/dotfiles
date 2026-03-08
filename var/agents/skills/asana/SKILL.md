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

## Teams

### List Teams in Workspace

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/workspaces/WORKSPACE_GID/teams" | jq '.data'
```

### Get Team

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/teams/TEAM_GID" | jq '.data'
```

**Response**:
```json
{
  "gid": "1234567890123456",
  "name": "Engineering",
  "resource_type": "team",
  "description": "Engineering team"
}
```

### List Users in Team

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/teams/TEAM_GID/users" | jq '.data'
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

### List Projects in Team

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/teams/TEAM_GID/projects?archived=false" | jq '.data'
```

## Sections (Columns/Board)

### List Sections in Project

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/projects/PROJECT_GID/sections" | jq '.data'
```

**Response**:
```json
[
  {
    "gid": "1234567890123456",
    "name": "To Do",
    "resource_type": "section"
  }
]
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
  "custom_fields": [{ "gid": "...", "name": "Priority", "enum_value": { "name": "High" } }],
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
| `opt_fields` | Specific fields to return (comma-separated) |

### Search Tasks (Workspace Level)

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
| `completed` | Filter by completion status (`true`/`false`) |
| `due_on.before` | Due before date (YYYY-MM-DD) |
| `due_on.after` | Due after date (YYYY-MM-DD) |
| `due_on` | Due on specific date (YYYY-MM-DD) |
| `modified_at.before` | Modified before timestamp |
| `modified_at.after` | Modified after timestamp |
| `created_at.before` | Created before timestamp |
| `created_at.after` | Created after timestamp |
| `is_subtask` | Filter subtasks (`true`/`false`) |

## Subtasks

### Get Subtasks

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/tasks/TASK_GID/subtasks" | jq '.data'
```

## Stories (Comments)

### Get Task Stories (Comments)

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/tasks/TASK_GID/stories" | jq '.data'
```

**Response**:
```json
[
  {
    "gid": "1234567890123456",
    "resource_type": "story",
    "type": "comment",
    "text": "This is a comment",
    "created_by": { "gid": "...", "name": "..." },
    "created_at": "2025-01-15T10:30:00.000Z"
  }
]
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

### Get Tag

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/tags/TAG_GID" | jq '.data'
```

## Custom Fields

### List Custom Fields in Workspace

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/workspaces/WORKSPACE_GID/custom_fields" | jq '.data'
```

**Response**:
```json
[
  {
    "gid": "1234567890123456",
    "name": "Priority",
    "resource_type": "custom_field",
    "type": "enum",
    "enum_options": [
      { "gid": "...", "name": "Low", "color": "blue" },
      { "gid": "...", "name": "Medium", "color": "yellow" },
      { "gid": "...", "name": "High", "color": "red" }
    ]
  }
]
```

### Get Custom Field Settings for Project

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/projects/PROJECT_GID/custom_field_settings" | jq '.data'
```

## Attachments

### Get Task Attachments

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/tasks/TASK_GID/attachments" | jq '.data'
```

**Response**:
```json
[
  {
    "gid": "1234567890123456",
    "name": "document.pdf",
    "resource_type": "attachment",
    "download_url": "https://...",
    "view_url": "https://...",
    "host": "asana",
    "created_at": "2025-01-15T10:30:00.000Z"
  }
]
```

### Get Attachment

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/attachments/ATTACHMENT_GID" | jq '.data'
```

## Portfolios

### List Portfolios

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/portfolios?workspace=WORKSPACE_GID" | jq '.data'
```

### Get Portfolio Items

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/portfolios/PORTFOLIO_GID/items" | jq '.data'
```

## Batch API

### Submit Batch Request

Execute multiple GET requests in a single call:

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://app.asana.com/api/1.0/batch" \
  -d '{
    "actions": [
      { "method": "GET", "relative_path": "/tasks/TASK_GID_1" },
      { "method": "GET", "relative_path": "/tasks/TASK_GID_2" },
      { "method": "GET", "relative_path": "/tasks/TASK_GID_3" }
    ]
  }' | jq '.data'
```

**Response**:
```json
[
  {
    "headers": { "...": "..." },
    "body": { "data": { "gid": "...", "name": "..." } },
    "status_code": 200
  }
]
```

## Common ID Patterns

- All resources use GIDs (globally unique identifiers) - numeric strings like "1201234567890123"
- Task GIDs, Project GIDs, Team GIDs all follow the same format
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

## Opt Fields for Efficiency

Request only needed fields to reduce payload size:

```bash
curl -s -H "Authorization: Bearer $ASANA_PAT" \
  "https://app.asana.com/api/1.0/projects/PROJECT_GID/tasks?opt_fields=name,assignee.name,due_on,completed" | jq '.data'
```

Common opt_fields values:
- `name`, `notes`, `completed`, `due_on`, `due_at`
- `assignee.name`, `assignee.gid`
- `projects.name`, `projects.gid`
- `tags.name`, `tags.gid`
- `custom_fields.name`, `custom_fields.enum_value.name`
- `memberships.section.name`

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
6. **Batch requests**: Use batch API for multiple independent GET requests

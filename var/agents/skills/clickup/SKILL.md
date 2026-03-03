---
name: clickup
description: Interact with ClickUp API for task management, workspace hierarchy, and document access using CLICKUP_PAT environment variable.
---

ClickUp API v2 reference for common operations.

## Prerequisites

- `CLICKUP_PAT` environment variable with a valid ClickUp Personal Access Token

**Check before use:**
```bash
[ -z "$CLICKUP_PAT" ] && echo "Error: CLICKUP_PAT not set" || echo "OK: CLICKUP_PAT is set"
```

**Note:** Never hardcode `CLICKUP_PAT`. Always use the environment variable.

## Base URL

```
https://api.clickup.com/api/v2
```

## Authentication

All requests require the `Authorization` header:

```bash
curl -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/..."
```

## Workspaces

### List Workspaces

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team" | jq .
```

**Response**:
```json
{
  "teams": [
    {
      "id": "12345678",
      "name": "My Workspace",
      "color": "#000000",
      "avatar": "..."
    }
  ]
}
```

## Workspace Hierarchy

### Get Full Hierarchy

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/space?archived=false" | jq .
```

### Get Spaces

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/space?archived=false" | jq .
```

### Get Folders (in a Space)

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/space/SPACE_ID/folder?archived=false" | jq .
```

### Get Lists (in a Space)

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/space/SPACE_ID/list?archived=false" | jq .
```

### Get Lists (in a Folder)

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/folder/FOLDER_ID/list?archived=false" | jq .
```

## Tasks

### Get Task

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/task/TASK_ID" | jq .
```

**Response**:
```json
{
  "id": "ABC123DEF",
  "name": "Task Name",
  "description": "Task description...",
  "status": { "status": "in progress", "color": "#5f55ee" },
  "priority": { "priority": "high", "color": "#ff0000" },
  "assignees": [{ "id": 123, "username": "john" }],
  "due_date": "1640995200000",
  "tags": [{ "name": "urgent" }],
  "url": "https://app.clickup.com/t/ABC123DEF"
}
```

### List Tasks

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/list/LIST_ID/task?archived=false&subtasks=true" | jq .
```

**Query Parameters**:
| Parameter | Description |
|-----------|-------------|
| `archived` | Include archived tasks (`true`/`false`) |
| `subtasks` | Include subtasks (`true`/`false`) |
| `statuses[]` | Filter by status (e.g., `statuses[]=in progress`) |
| `assignees[]` | Filter by assignee ID |
| `due_date_gt` | Due date greater than (timestamp) |
| `due_date_lt` | Due date less than (timestamp) |
| `limit` | Max results (default 100, max 100) |
| `page` | Page number for pagination |

### Search Tasks

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/task?search=KEYWORD&archived=false" | jq .
```

## Documents

### List Documents (Pages)

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/workflow/WORKFLOW_ID/doc" | jq .
```

### Get Document Content

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/workflow/WORKFLOW_ID/doc/DOC_ID/page/PAGE_ID" | jq .
```

## Comments

### Get Task Comments

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/task/TASK_ID/comment" | jq .
```

## Time Tracking

### Get Time Entries for Task

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/task/TASK_ID/time" | jq .
```

### Get Current Time Entry

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/time_entries/current" | jq .
```

## Members

### Get Workspace Members

```bash
curl -s -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/member" | jq '.members'
```

## Common ID Patterns

- `CU-<ID>` in code/branches refers to ClickUp task `<ID>`
- Task IDs are strings (e.g., "ABC123DEF", "86abc4d12")
- Numeric IDs are used for teams, spaces, folders, lists

## Utility: Extract Task ID from URL

```bash
# From https://app.clickup.com/t/ABC123DEF/86abc4d12
# Task ID is: 86abc4d12

echo "https://app.clickup.com/t/ABC123DEF/86abc4d12" | sed 's/.*\///'
# Output: 86abc4d12
```

## Error Handling

| Status | Meaning |
|--------|---------|
| `200` | Success |
| `400` | Bad request (invalid parameters) |
| `401` | Unauthorized (invalid token) |
| `403` | Forbidden (insufficient permissions) |
| `404` | Not found |
| `429` | Rate limit exceeded |

## Best Practices

1. **Default to incomplete tasks**: Unless explicitly asked for completed ones
2. **Use search when IDs unknown**: Start with `search` or list endpoints
3. **Tasks organized in sprints**: Filter by list/folder for sprint context
4. **Documents are read-only**: Only read pages, do not create or update

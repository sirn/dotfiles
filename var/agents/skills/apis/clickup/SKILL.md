---
name: clickup
description: Interact with the ClickUp HTTP API using a Personal Access Token in CLICKUP_PAT. Covers v2 core resources plus v3 Docs, Chat, attachments, ACLs, and audit logs. Use the API directly, not MCP.
---

Use ClickUp's HTTP API directly, not MCP.

This skill is **PAT-only**:

- Use a ClickUp **Personal Access Token** in `CLICKUP_PAT`
- Send it as `Authorization: $CLICKUP_PAT` (do not use `Bearer` for PAT auth)
- Personal tokens begin with `pk_`

## Canonical references

- Main docs: `https://developer.clickup.com/`
- Authentication: `https://developer.clickup.com/docs/authentication`
- Raw OpenAPI v2: `https://developer.clickup.com/openapi/clickup-api-v2-reference.json`
- Raw OpenAPI v3: `https://developer.clickup.com/openapi/ClickUp_PUBLIC_API_V3.yaml`

## Base URLs

- v2 core API: `https://api.clickup.com/api/v2`
- v3 API: `https://api.clickup.com/api/v3`

## Quick auth check

```bash
[ -z "$CLICKUP_PAT" ] && echo "Error: CLICKUP_PAT not set" || echo "OK: CLICKUP_PAT is set"

curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/user" | jaq .
```

## API coverage map

ClickUp exposes two public API surfaces:

### v2: core work management

Use v2 for most existing ClickUp resources:

- Authorized user + workspaces
- Spaces, folders, lists, views
- Tasks, task relationships, checklists, attachments
- Comments
- Members, users, guests, user groups, roles
- Custom fields, tags, custom task types
- Goals
- Time tracking
- Templates
- Shared hierarchy
- Webhooks

### v3: newer resources

Use v3 for the newer resource families:

- Docs
- Chat
- Generic attachments endpoints
- Privacy/access control lists (ACLs)
- Audit logs
- A few workspace-scoped task helpers

## Important ID note

ClickUp uses both `team_id` and `workspace_id` across docs/specs; both refer to the same top-level workspace identifier. The v2 spec also inconsistently uses both `team_id` and `team_Id` in path templates. Treat them as the same value.

## Recommended discovery flow

When IDs are unknown, discover them in this order:

1. Get the authorized user
2. List workspaces
3. List spaces in a workspace
4. List folders and folderless lists in a space
5. List lists in a folder
6. List views if needed
7. Query tasks, comments, docs, chat, time entries, etc.

## Authorized user and workspaces

### Get authorized user

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/user" | jaq .
```

### List authorized workspaces

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team" | jaq '.teams'
```

### Get workspace plan

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/plan" | jaq .
```

### Get workspace seats

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/seats" | jaq .
```

## Hierarchy: spaces, folders, lists, views

### List spaces in a workspace

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/space?archived=false" | jaq .
```

### Get a space

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/space/SPACE_ID" | jaq .
```

### List folders in a space

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/space/SPACE_ID/folder?archived=false" | jaq .
```

### Get a folder

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/folder/FOLDER_ID" | jaq .
```

### List folderless lists in a space

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/space/SPACE_ID/list?archived=false" | jaq .
```

### List lists in a folder

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/folder/FOLDER_ID/list?archived=false" | jaq .
```

### Get a list

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/list/LIST_ID" | jaq .
```

### List views

```bash
# Workspace / Everything views
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/view" | jaq .

# Space views
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/space/SPACE_ID/view" | jaq .

# Folder views
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/folder/FOLDER_ID/view" | jaq .

# List views
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/list/LIST_ID/view" | jaq .
```

### Get a view or its tasks

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/view/VIEW_ID" | jaq .
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/view/VIEW_ID/task" | jaq .
```

## Tasks

### Get a task

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/task/TASK_ID" | jaq .
```

Useful query parameters:

- `include_subtasks=true`
- `include_markdown_description=true`
- `custom_fields=true`
- `custom_task_ids=true&team_id=TEAM_ID` when using custom task IDs

Example:

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/task/TASK_ID?include_subtasks=true&include_markdown_description=true" | jaq .
```

### List tasks in a list

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/list/LIST_ID/task?archived=false&subtasks=true&include_closed=false" | jaq .
```

Common filters supported by `GET /list/{list_id}/task`:

- `page`
- `order_by`
- `reverse`
- `statuses[]`
- `assignees[]`
- `watchers[]`
- `tags[]`
- `subtasks`
- `include_closed`
- `due_date_gt`, `due_date_lt`
- `date_created_gt`, `date_created_lt`
- `date_updated_gt`, `date_updated_lt`
- `date_done_gt`, `date_done_lt`
- `custom_fields`
- `custom_items[]`

### Get filtered workspace tasks

This is the main workspace-level task query in v2. It filters tasks across spaces, folders, and lists.

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/task?include_closed=false&page=0" | jaq .
```

Common filters supported by `GET /team/{team_Id}/task`:

- `page`
- `order_by`
- `reverse`
- `subtasks`
- `space_ids[]`
- `project_ids[]`
- `list_ids[]`
- `statuses[]`
- `assignees[]`
- `tags[]`
- `parent`
- `include_closed`
- `include_markdown_description`
- `custom_fields`
- `custom_items[]`
- `due_date_gt`, `due_date_lt`
- `date_created_gt`, `date_created_lt`
- `date_updated_gt`, `date_updated_lt`
- `date_done_gt`, `date_done_lt`

### Create or update tasks

> **Always use `markdown_content` for task descriptions (both create and update).** The `description` field treats content as plain text and does not render Markdown, whereas `markdown_content` renders rich text in the ClickUp UI (`markdown_description` is only a response field when `include_markdown_description=true` is passed on `GET`). If both `markdown_content` and `description` are provided, `markdown_content` takes precedence.

```bash
# Create task in a list (with Markdown description)
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v2/list/LIST_ID/task" \
  -d '{"name":"New task","markdown_content":"## Overview\nDetails here."}' \
  | jaq .

# Update task (with Markdown description)
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X PUT \
  "https://api.clickup.com/api/v2/task/TASK_ID" \
  -d '{"name":"Updated task name","markdown_content":"## Updated\nNew details."}' \
  | jaq .
```

### Task relationships

```bash
# Add dependency
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v2/task/TASK_ID/dependency" \
  -d '{"depends_on":"OTHER_TASK_ID"}' | jaq .

# Link tasks
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -X POST "https://api.clickup.com/api/v2/task/TASK_ID/link/OTHER_TASK_ID" | jaq .
```

### Checklists

```bash
# Create checklist
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v2/task/TASK_ID/checklist" \
  -d '{"name":"Checklist"}' | jaq .
```

### Task members

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/task/TASK_ID/member" | jaq .
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/list/LIST_ID/member" | jaq .
```

### Task attachment upload (v2)

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -F "attachment=@/path/to/file" \
  "https://api.clickup.com/api/v2/task/TASK_ID/attachment" | jaq .
```

## Comments

### Get task comments

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/task/TASK_ID/comment" | jaq .
```

Optional pagination-style params:

- `start`
- `start_id`
- `custom_task_ids=true&team_id=TEAM_ID` if using custom task IDs

### Get list comments

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/list/LIST_ID/comment" | jaq .
```

### Get view comments

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/view/VIEW_ID/comment" | jaq .
```

### Get threaded replies for a comment

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/comment/COMMENT_ID/reply" | jaq .
```

### Create comments

```bash
# Task comment
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v2/task/TASK_ID/comment" \
  -d '{"comment_text":"Hello from the API","notify_all":true}' | jaq .

# Threaded reply
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v2/comment/COMMENT_ID/reply" \
  -d '{"comment_text":"Replying in thread","notify_all":true}' | jaq .
```

## Custom fields, tags, custom task types, roles

### Custom fields

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/field" | jaq .
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/space/SPACE_ID/field" | jaq .
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/folder/FOLDER_ID/field" | jaq .
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/list/LIST_ID/field" | jaq .
```

### Set a task custom field value

The body shape depends on the field type. For simple text-like fields:

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v2/task/TASK_ID/field/FIELD_ID" \
  -d '{"value":"example"}' | jaq .
```

### Tags

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/space/SPACE_ID/tag" | jaq .
```

```bash
# Add tag to task
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -X POST "https://api.clickup.com/api/v2/task/TASK_ID/tag/TAG_NAME" | jaq .

# Remove tag from task
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -X DELETE "https://api.clickup.com/api/v2/task/TASK_ID/tag/TAG_NAME" | jaq .
```

### Custom task types

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/custom_item" | jaq .
```

### Custom roles

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/customroles?include_members=true" | jaq .
```

## Goals

### List goals

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/goal?include_completed=false" | jaq .
```

### Get a goal

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/goal/GOAL_ID" | jaq .
```

### Key results

Key results are returned inside `GET /api/v2/goal/GOAL_ID` (`goal.key_results`). To create, update, or delete a key result:

```bash
# Create a key result on a goal
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v2/goal/GOAL_ID/key_result" \
  -d '{"name":"Key result","type":"number","steps_start":0,"steps_end":10}' | jaq .

# Update a key result
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X PUT \
  "https://api.clickup.com/api/v2/key_result/KEY_RESULT_ID" \
  -d '{"steps_current":5,"note":"Progress update"}' | jaq .
```

## Time tracking

### Get time entries in a date range

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/time_entries?start_date=START_MS&end_date=END_MS" | jaq .
```

Useful filters:

- `assignee`
- `space_id`
- `folder_id`
- `list_id`
- `task_id`
- `is_billable`
- `include_task_tags=true`
- `include_location_names=true`
- `include_approval_history=true`
- `include_approval_details=true`

### Get the currently running time entry

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/time_entries/current" | jaq .
```

### Get a single time entry or its history

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/time_entries/TIMER_ID" | jaq .
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/time_entries/TIMER_ID/history" | jaq .
```

### Start or stop a timer

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v2/team/TEAM_ID/time_entries/start" \
  -d '{"description":"Working on task"}' | jaq .

curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v2/team/TEAM_ID/time_entries/stop" \
  -d '{}' | jaq .
```

### Legacy task time tracking endpoints

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/task/TASK_ID/time" | jaq .
```

## Templates, shared hierarchy, members, users, guests, groups

### Shared hierarchy

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/shared" | jaq .
```

### Templates

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/taskTemplate?page=0" | jaq .
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/list_template" | jaq .
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/folder_template" | jaq .
```

### Users, guests, groups

```bash
# User details in workspace
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/user/USER_ID" | jaq .

# Guest details in workspace
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/guest/GUEST_ID" | jaq .

# List user groups (team_id is required)
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/group?team_id=TEAM_ID" | jaq .
```

## Webhooks

### List workspace webhooks

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v2/team/TEAM_ID/webhook" | jaq .
```

### Create a webhook

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v2/team/TEAM_ID/webhook" \
  -d '{"endpoint":"https://example.com/clickup-webhook","events":["taskCreated"]}' | jaq .
```

## Docs API (v3)

Docs live in **v3**. Avoid using incorrect `/workflow/.../doc` style URLs, and use workspace-scoped v3 endpoints instead.

### Search docs in a workspace

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/docs?archived=false&deleted=false&limit=50" | jaq .
```

Useful filters:

- `id`
- `creator`
- `deleted`
- `archived`
- `parent_id`
- `parent_type`
- `limit`
- `cursor`
- `next_cursor`

### Fetch a doc

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/docs/DOC_ID" | jaq .
```

### Get doc page listing

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/docs/DOC_ID/page_listing?max_page_depth=10" | jaq .
```

### Get doc pages

`content_format` supports `text/md` and `text/plain`.

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/docs/DOC_ID/pages?content_format=text/md" | jaq .
```

### Get a specific page

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/docs/DOC_ID/pages/PAGE_ID?content_format=text/md" | jaq .
```

### Create docs/pages

```bash
# Create a doc
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/docs" \
  -d '{"name":"New doc"}' | jaq .

# Create a page
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/docs/DOC_ID/pages" \
  -d '{"name":"New page"}' | jaq .
```

## Chat API (v3)

### List chat channels

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/chat/channels?limit=50" | jaq .
```

Useful filters:

- `description_format=text/md|text/plain`
- `cursor`
- `limit`
- `is_follower=true`
- `include_closed=true`
- `with_message_since=<timestamp>`
- `channel_types=<value>`

### Get a channel

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/chat/channels/CHANNEL_ID" | jaq .
```

### Get channel followers or members

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/chat/channels/CHANNEL_ID/followers" | jaq .
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/chat/channels/CHANNEL_ID/members" | jaq .
```

### Get channel messages

`content_format` supports `text/md` and `text/plain`.

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/chat/channels/CHANNEL_ID/messages?limit=50&content_format=text/md" | jaq .
```

### Get replies or reactions for a message

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/chat/messages/MESSAGE_ID/replies?content_format=text/md" | jaq .
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/chat/messages/MESSAGE_ID/reactions" | jaq .
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/chat/messages/MESSAGE_ID/tagged_users" | jaq .
```

### Send a message

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/chat/channels/CHANNEL_ID/messages" \
  -d '{"type":"message","content":"Hello from the API","content_format":"text/plain"}' | jaq .
```

## Generic attachments API (v3)

### List attachments for a supported entity

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/ENTITY_TYPE/ENTITY_ID/attachments?limit=50" | jaq .
```

### Upload an attachment to a supported entity

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -F "attachment=@/path/to/file" \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/ENTITY_TYPE/ENTITY_ID/attachments" | jaq .
```

## Privacy / ACLs (v3)

### Update access control on an object or location

The v3 schema supports `private` and `entries`.

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X PATCH \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/OBJECT_TYPE/OBJECT_ID/acls" \
  -d '{"private":true}' | jaq .
```

See the v3 spec for the exact body shape for the target object type.

## Audit logs (v3)

### Query workspace audit logs

The audit logs endpoint is a `POST` in v3 (`applicability` values include `auth-and-security`, `user-activity`, `hierarchy-activity`, `custom-fields`, `agent-settings-activity`, `other-activity`).

```bash
curl -fsS \
  -H "Authorization: $CLICKUP_PAT" \
  -H "Content-Type: application/json" \
  -X POST \
  "https://api.clickup.com/api/v3/workspaces/WORKSPACE_ID/auditlogs" \
  -d '{"applicability":"auth-and-security"}' | jaq .
```

## Common URL / ID patterns

- Workspace IDs are numeric strings
- Space, folder, list, and view IDs are numeric strings
- Task IDs can be short alphanumeric IDs
- Custom task IDs require `custom_task_ids=true&team_id=TEAM_ID`
- Docs and chat resources use `workspace_id` in v3

### Extract a task ID from a ClickUp task URL

```bash
echo "https://app.clickup.com/t/123456/86abc4d12" | sed 's#.*/##'
# => 86abc4d12
```

## Pagination patterns

### v2

v2 commonly uses page-based pagination:

- `page=0`
- `page=1`
- plus resource-specific filters

### v3

v3 commonly uses cursor pagination:

- `limit`
- `cursor`
- sometimes `next_cursor`

## Error handling

| Status | Meaning                      |
| ------ | ---------------------------- |
| `200`  | Success                      |
| `201`  | Created                      |
| `400`  | Bad request / invalid params |
| `401`  | Unauthorized / invalid PAT   |
| `403`  | Forbidden                    |
| `404`  | Not found                    |
| `429`  | Rate limited                 |
| `5xx`  | Server error                 |

Prefer `curl -fsS` so HTTP failures surface immediately.

## Best practices

1. **Use PAT auth only for this skill**: `Authorization: $CLICKUP_PAT`
2. **Use the HTTP API directly, not MCP**
3. **Start with hierarchy discovery**: workspace → space → folder/list → task/view/doc/channel
4. **Use v2 for core work items** and **v3 for Docs/Chat/ACLs/Audit Logs**
5. **Default to incomplete tasks** unless the user explicitly asks for completed/closed ones
6. **When using custom task IDs**, add `custom_task_ids=true&team_id=TEAM_ID`
7. **Free-text task search is not a general v2 endpoint**: use workspace/list filtering or fetch and filter client-side
8. **Docs content and chat message content support `text/md` and `text/plain`**
9. **Use `markdown_content` for task descriptions (both create and update)** — `description` is plain text only and will not render Markdown (`markdown_description` is only a `GET` response field)
10. **Be careful with mutations**: creating, editing, deleting, ACL changes, and webhook changes should only be done when explicitly requested

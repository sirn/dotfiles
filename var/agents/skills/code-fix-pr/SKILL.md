---
name: code-fix-pr
description: Evaluate PR review comments, classify them by status, and create concrete fix plans. Use when user asks to address PR comments, fix review feedback, or handle code review comments.
---

Evaluate and address PR review comments by classifying them and creating a concrete fix plan.

## Prerequisites

**Reference these skills first:**
- Read `jj-reference` skill for Jujutsu commands to understand change context
- Read `gh-reference` skill for GitHub CLI commands to fetch PR comments

## Process

1. **Identify the PR context**:
   - Get the PR number from user or infer from current branch
   - Determine repository: `gh repo view` or `jj git remote list`
   - Use explicit `-R owner/repo` with all `gh` commands

2. **Fetch PR data** (read-only GET requests only):
   - Get PR details: `gh pr view <number> -R owner/repo --json number,title,state,url`
   - Get PR diff: `gh pr diff <number> -R owner/repo`
   - Fetch review comments: `gh api repos/owner/repo/pulls/<number>/comments -X GET --paginate`
   - Fetch review threads: `gh api repos/owner/repo/pulls/<number>/reviews -X GET --paginate`
   - Check PR checks: `gh pr checks <number> -R owner/repo`
   
   **Important**: Always use `-X GET` (or `--method GET`) to be explicit about read-only access. This ensures commands are allowed by the safety policy.

3. **Check GitHub Actions** (if checks are failing):
   - List workflow runs for PR branch: `gh run list -R owner/repo --branch <branch> -L 5`
   - View failed run details: `gh run view <run-id> -R owner/repo --log-failed`
   - View specific job logs: `gh run view <run-id> -R owner/repo --job <job-id> --log`

4. **Analyze the codebase**:
   - Read relevant files mentioned in comments
   - Run `jj diff -s` to see current working copy changes
   - Understand the current state of the code

5. **Classify each comment**:
   - **Already Addressed** - Changes in working copy or commits resolve the comment
   - **Valid (Should Fix)** - Legitimate issue requiring a fix
   - **Not Valid (Won't Fix)** - Incorrect or inapplicable feedback with reasoning
   - **Needs Discussion** - Unclear or requires clarification from reviewer

6. **Research best practices** (for valid comments):
   - Use `brave-search` or `synthetic-search` to verify patterns
   - Look up official documentation for APIs/frameworks involved
   - Research idiomatic solutions for the language/project

7. **Create fix plan**:
   - Prioritize critical/security issues first
   - Group related fixes for efficiency
   - Provide specific file:line references

## Output

1. **PR Summary**
   - PR number, title, state
   - Total comments analyzed

2. **Comment Classification**

   **Already Addressed** (✓):
   - List comments with evidence of resolution
   - Reference commit or code state

   **Valid - Should Fix** (!):
   - List with severity (Critical/High/Low)
   - Original comment text
   - File:line location
   - Why it's valid

   **Not Valid - Won't Fix** (⊘):
   - List with reasoning for each
   - Why the feedback doesn't apply

   **Needs Discussion** (?):
   - List with specific questions to ask reviewer

3. **Fix Plan**
   - Numbered steps with file:line targets
   - Specific changes to make
   - Best practice references (with sources)
   - Estimated effort

4. **Next Steps**
   - Immediate actions
   - Questions for reviewer
   - Follow-up after fixes

## Important

- **Never** push fixes without explicit user confirmation
- Ask before making destructive changes (deletions, major refactors)
- Use `jj` commands for all VCS operations (refer to `jj-reference`)
- For uncertain classifications, default to "Needs Discussion"

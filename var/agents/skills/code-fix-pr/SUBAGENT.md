---
name: code-fix-pr
description: Evaluate PR review comments using specialized agents for parallel analysis of different comment categories.
---

Evaluate and address PR review comments by delegating to specialized agents.

## Prerequisites

**Reference these skills first:**
- Read `jj-reference` skill for Jujutsu commands to understand change context
- Read `gh-reference` skill for GitHub CLI commands to fetch PR comments

## Process

1. **Gather PR data** (sequential):
   - Get PR number from user or infer from branch
   - Determine repository: `gh repo view` or `jj git remote list`
   - Fetch PR comments: `gh api repos/owner/repo/pulls/<number>/comments -X GET --paginate`
   - Fetch reviews: `gh api repos/owner/repo/pulls/<number>/reviews -X GET --paginate`
   - Get PR diff: `gh pr diff <number> -R owner/repo`
   - Check PR status: `gh pr checks <number> -R owner/repo`
   - If checks failing, fetch logs: `gh run view <run-id> -R owner/repo --log-failed`
   - Run `jj diff -s` to see current working copy changes
   
   **Important**: Always use `-X GET` to be explicit about read-only access.

2. **Spawn parallel agents** for comment analysis:
   - `quality-reviewer`: "Review these PR comments for bugs, logic errors, and quality issues. Classify each as: already-addressed, valid-fix-needed, invalid, or needs-discussion."
   - `security-researcher`: "Review these PR comments for security-related feedback. Identify valid security concerns vs false positives."
   - `simplicity-reviewer`: "Review these PR comments for complexity concerns. Flag over-engineering suggestions vs valid simplifications."
   - `code-researcher`: "Research best practices for the fixes suggested in these PR comments. Provide authoritative sources."

3. **Synthesize findings** into unified report

## Output

1. **PR Summary**
   - PR number, title, state
   - Total comments analyzed

2. **Comment Classification**
   - **Already Addressed**: List with resolution evidence
   - **Valid - Should Fix**: List with severity and location
   - **Not Valid - Won't Fix**: List with reasoning
   - **Needs Discussion**: List with questions

3. **Fix Plan**
   - Numbered steps with file:line targets
   - Best practice references
   - Estimated effort

4. **Next Steps**
   - Immediate actions
   - Questions for reviewer

## Agent Roles

**quality-reviewer**:
- Analyze each comment for bugs, logic errors, and quality issues
- Determine if already addressed by existing changes
- Validate technical accuracy of feedback
- Flag unclear or ambiguous comments

**security-researcher**:
- Identify security-related comments
- Validate security concerns vs false positives
- Assess severity of security issues

**simplicity-reviewer**:
- Evaluate complexity-related comments
- Distinguish valid simplifications from over-engineering suggestions
- Recommend pragmatic approaches

**code-researcher**:
- Use WebSearch/WebFetch to verify patterns
- Look up official documentation
- Research idiomatic solutions
- Provide authoritative sources

## Important

- **Never** push fixes without explicit user confirmation
- Ask before destructive changes
- Use `jj` commands for VCS operations (refer to `jj-reference`)

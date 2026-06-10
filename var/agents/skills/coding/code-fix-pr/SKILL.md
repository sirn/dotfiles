---
name: code-fix-pr
description: Evaluate PR review comments with parallel analysis across correctness, security, and convention lenses.
---

Evaluate and address PR review comments by delegating to expert roles.

## Prerequisites

**Reference these skills first:**

- Read `jujutsu` skill for Jujutsu commands to understand change context
- Read `github-cli` skill for GitHub CLI commands to fetch PR comments

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

2. Spawn parallel agents for comment analysis:
   - `reviewer`: "Review these PR comments across all lenses: (1) correctness/quality — classify each as already-addressed, valid-fix-needed, invalid, or needs-discussion; (2) security — identify valid security concerns vs false positives and assess severity; (3) simplicity/convention — prioritize the simplest possible change that satisfies valid feedback, distinguish valid simplifications from over-engineering suggestions."
   - `researcher`: "Research best practices and official documentation for the fixes suggested in these PR comments. Provide authoritative sources."

3. Use `oracle` only for disputed comments or conflicting expert recommendations:
   - `oracle`: "Adjudicate these disputed PR comments and conflicting recommendations. Decide which feedback should be fixed, discussed, or rejected, and explain why."

4. **Synthesize findings** into unified report.

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

**reviewer**:

- Analyze comments through the requested lens: correctness, security, simplicity, convention, or plan/design.
- Determine if feedback is already addressed by existing changes.
- Validate technical accuracy of feedback.
- Flag unclear or ambiguous comments.
- Recommend pragmatic minimal fixes for valid feedback.

**researcher**:

- Use WebSearch/WebFetch to verify patterns.
- Look up official documentation.
- Research idiomatic solutions.
- Provide authoritative sources.

**oracle**:

- Resolve disputed comments or conflicting recommendations.
- State assumptions, tradeoffs, and confidence.

## Important

- **Never** push fixes without explicit user confirmation
- Ask before destructive changes
- Use `jj` commands for VCS operations (refer to `jujutsu`)

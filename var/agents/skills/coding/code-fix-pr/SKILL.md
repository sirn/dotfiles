---
name: code-fix-pr
description: Evaluate PR review comments with parallel analysis across correctness, security, and convention lenses.
---

Evaluate and address PR review comments by delegating to expert roles.

## Prerequisites

**Reference these skills first:**

- Read `jujutsu` skill to understand change context
- Read `github-cli` skill to fetch PR comments

## Process

### Step 1 - Gather PR Data

- Get PR number: ask the user or infer from the current branch
- Determine repository: run `gh repo view` or `jj git remote list`
- Fetch PR comments: run `gh api repos/owner/repo/pulls/<number>/comments -X GET --paginate`
- Fetch reviews: run `gh api repos/owner/repo/pulls/<number>/reviews -X GET --paginate`
- Get PR diff: run `gh pr diff <number> -R owner/repo`
- Check PR status: run `gh pr checks <number> -R owner/repo`
- Fetch failure logs: run `gh run view <run-id> -R owner/repo --log-failed`
- Capture CI failure context: summarize failed checks, error messages, and stack traces for subsequent analysis
- View working copy changes: run `jj diff -s`

**Important**: Always use `-X GET` to be explicit about read-only access.

### Step 2 - Analyze Comments and CI

Spawn `reviewer` subagent for review comments:

```
Review these PR comments across all lenses:

- correctness/quality — classify each as already-addressed, valid-fix-needed, invalid, or needs-discussion
- security — identify valid security concerns vs false positives and assess severity
- simplicity/convention — prioritize the simplest possible change that satisfies valid feedback, distinguish valid simplifications from over-engineering suggestions
- ci-correlation — for each comment, check whether it relates to an observed CI failure; if so, note the connection and whether the comment would resolve the failure

Include the CI failure context below when available.
```

Spawn `researcher` subagent for API validation/best practices:

```
Research best practices and official documentation for the fixes suggested in these PR comments. Provide authoritative sources. If CI failure logs are included, research the specific error messages, stack traces, or test failures to find root causes and known solutions.
```

Spawn `reviewer` subagent for CI analysis only if checks failed:

```
Analyze these GitHub Actions CI failure logs:

- failure-classification — classify each as test-flake, code-regression, env/infra-issue, dependency-problem, or timeout
- root-cause — identify the most likely root cause for each failure with supporting evidence from logs
- diff-relationship — check whether failures are in code touched by this PR's diff or in unrelated code
- fix-suggestion — for code-regression failures, suggest minimal fixes; for flake/infra issues, suggest mitigation (retry, environment fix, etc.)
```

### Step 3 - Adjudicate Disputes

Spawn `oracle` subagent for any disputes:

```
Adjudicate these disputed PR comments and conflicting recommendations. Decide which feedback should be fixed, discussed, or rejected, and explain why.
```

### Step 4 - Synthesize Findings

Synthesize the analysis into a unified report with the following sections:

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

4. **CI Failure Analysis** — only include if checks failed
   - **Failing Checks**: List each failed check with run ID and error summary
   - **Failure Classification**: Test flake / code regression / env-infra / dependency / timeout
   - **Root Causes**: Per-failure analysis with log evidence
   - **PR Relationship**: Whether each failure is in PR-touched code or unrelated
   - **Recommendations**: Fix suggestions for regressions, mitigation steps for flakes/infra

5. **Next Steps**
   - Immediate actions
   - Questions for reviewer

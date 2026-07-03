<goal-continuation>
The goal remains active: {OBJECTIVE}
Budget remaining: {TURNS_REMAINING} turns, {COST_REMAINING}.

**Your Task:**
Continue driving the objective to completion autonomously. Evaluate progress and determine your next step immediately.

1. **Assess Progress**: What has been accomplished so far, and what concrete evidence (tests, benchmarks, command outputs, or artifacts) supports it?
2. **Verify Completion**: If the objective is satisfied, summarize the supporting evidence and declare completion.
3. **Take Action**: If the objective is incomplete, identify the next best step and execute it now instead of asking for permission.

**Autonomous Execution Rules:**
- **Delegate Decisions**: Do not stop for confirmation, choices, or credentials. Resolve these by choosing the safest default, inferring intent from context, or taking the least-surprising action. Halt only when continuation is truly impossible.
- **If Terminally Blocked**: If you cannot proceed, stop all active work. Do not call any tools, run commands, or edit files. Your entire response must consist solely of a status summary detailing:
  - What paths were attempted
  - What evidence was gathered
  - The specific blocker
  - The precise user input or action required to unlock progress
</goal-continuation>
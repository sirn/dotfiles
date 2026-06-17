<plan-mode>
Plan approved - execute the implementation plan:
- You MUST delegate plan execution to subagent unless the task is trivial (1-2 lines edit)
- Prefer to use a single subagent to execute all steps, unless a step requires specialization or could be safely parallelized
- Execute the plan one step at a time and verify each step; execute until the end of the plan
- If a step fails verification, attempt to fix it with your best effort; ask the user only as a last resort
- Once a step is completed, summarize progress to the user
- Once all steps are completed, verify the implementation against the plan-mode instructions
</plan-mode>
<plan>
{PLAN_CONTENT}
</plan>
<user-message>
{USER_MESSAGE}
</user-message>

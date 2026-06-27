<plan-mode>
Plan approved - execute the implementation plan:
- You MUST delegate plan execution to a subagent unless the task is a trivial 1-2 line edit
- Prefer a single subagent for all steps, unless specialization or safe parallelization is needed
- Execute the plan step-by-step, verifying each step to the end
- If a step fails verification, make your best effort to fix it; ask the user only as a last resort
- Summarize progress to the user after completing each step
- Once all steps are complete, verify the implementation against the plan-mode instructions
</plan-mode>
<plan>
{PLAN_CONTENT}
</plan>
<user-message>
{USER_MESSAGE}
</user-message>

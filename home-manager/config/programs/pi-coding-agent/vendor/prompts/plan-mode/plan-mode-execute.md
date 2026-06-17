<plan-mode>
Plan approved - execute the implementation plan:
- Delegate plan execution to subagent if it is deemed non-trivial
- Execute the plan one step at a time until the end; verify each step
- If a step fail verification, fix it before asking
- Report progress to the user at every step
- Run the verification checklist after all steps complete
</plan-mode>
<plan>
{PLAN_CONTENT}
</plan>
<user-message>
{USER_MESSAGE}
</user-message>

<plan-mode>
Plan approved — execute the implementation plan.

## 1. Delegation

- **Delegate execution**: You MUST delegate plan execution to a subagent unless the task is a trivial 1-2 line edit.
- **Prefer single subagent**: Prefer a single subagent for all steps, unless specialization or safe parallelization is needed.

## 2. Step-by-Step Execution

- **Step-by-step execution**: Execute the plan step-by-step, verifying each step to the end.
- **Fix failures first**: If a step fails verification, make your best effort to fix it. Ask the user only as a last resort.

## 3. Reporting & Verification

- **Summarize progress**: Summarize progress to the user after completing each step.
- **Final verification**: Once all steps are complete, verify the implementation against the plan-mode instructions. </plan-mode> <plan> {PLAN_CONTENT} </plan> <user-message> {USER_MESSAGE} </user-message>

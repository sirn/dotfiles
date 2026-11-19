---
name: terraform
type: reference
description: Local Terraform CLI safety and workflow guardrails (plan-then-confirm, -input=false, offline plan inspection in tmp/, state backups). ALWAYS read before running terraform commands; pair with the HashiCorp terraform-* / refactor-module skills for HCL authoring.
---

## Terraform CLI Safety & Workflow

Run `terraform <subcommand> -help` for exact CLI syntax and flags. Use the HashiCorp skills for authoring and refactoring (`terraform-style-guide`, `refactor-module`, `terraform-test`, `terraform-policy`, `terraform-search-import`, `terraform-stacks`). Apply these local operational rules whenever running `terraform` commands:

- **Plan-confirm-apply**: Run `terraform plan`, summarize all proposed creates, updates, and destroys, and get explicit user confirmation before running `terraform apply` or `terraform destroy`.
- **Restrict `-auto-approve`**: Never pass `-auto-approve` unless the user explicitly pre-approved applying that change.
- **Pass `-input=false`**: Always pass `-input=false` to `init`, `plan`, and `apply` in non-interactive shells so missing variables or backend settings fail immediately instead of hanging on stdin.
- **Save plans once and inspect offline**: Do not re-run `terraform plan` to grep or filter output. Save a binary plan file to `tmp/tfplan` once, then inspect it as text or JSON:

  ```bash
  mkdir -p tmp
  terraform plan -input=false -out=tmp/tfplan
  terraform show -no-color tmp/tfplan > tmp/tfplan.txt
  terraform show -json tmp/tfplan | jaq '.resource_changes[] | {address, actions: .change.actions}'
  ```

- **Verify workspace first**: Run `terraform workspace show` before any mutating or destructive command.
- **Prefer declarative refactoring blocks**: Use `moved {}`, `import {}`, and `removed {}` blocks (previewed in `terraform plan`) instead of imperative `terraform state mv`, `terraform import`, and `terraform state rm`. See `refactor-module` and `terraform-search-import`.
- **Back up state to `tmp/`**: Before running imperative `state mv` or `state rm`, save a backup under `tmp/` (never in the repository root):

  ```bash
  mkdir -p tmp
  terraform state pull > "tmp/state-backup-$(date +%Y%m%d).json"
  ```

- **Limit `-target` and `force-unlock`**: Reserve `-target` for emergencies (it leaves infrastructure inconsistent). Never run `terraform force-unlock` unless you confirmed the lock holder crashed.
- **Pin versions and locks**: Pin module versions in production and maintain `.terraform.lock.hcl` with `terraform providers lock`.

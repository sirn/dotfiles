---
name: web-design
type: reference
description: Local layout and spacing invariants for web UI (no external margins on components, container gap only, predefined token scales, no arbitrary Tailwind brackets). Use alongside `impeccable` and `modern-web-guidance` when writing or reviewing CSS, stylesheets, design tokens, or component templates.
---

## Layout & Spacing Invariants

Use `impeccable` for visual design craft, typography, accessibility, and critiques, and `modern-web-guidance` for modern web platform APIs. Apply these stricter local layout rules on top:

1. **Never use external margins on components or elements**:
   - Do not use `margin` to position or separate sibling elements.
   - Components must never push outward onto their surroundings.
   - **CSS**: Do not write `margin`, `margin-top`, `margin-bottom`, `margin-inline`, or `margin-block` on components or element selectors.
   - **Tailwind**: Do not use `m-*`, `mt-*`, `mb-*`, `ml-*`, `mr-*`, `mx-*`, or `my-*` on components.

2. **Elements can only change internal padding**:
   - Use `padding` exclusively for an element's own internal breathing room.
   - **CSS**: Set `padding`, `padding-inline`, or `padding-block` on the container or card element itself.
   - **Tailwind**: Use `p-*`, `px-*`, `py-*`, `pt-*`, `pb-*`, `pl-*`, or `pr-*`.

3. **Parent containers manage spacing with Flexbox or Grid gap**:
   - Use layout containers (`display: flex`, `display: grid`) with `gap` to separate child elements.
   - **CSS**: Use `display: flex; flex-direction: column; gap: var(--space-4);` or `display: grid; gap: var(--space-6);`.
   - **Tailwind**: Use `flex flex-col gap-4` or `grid gap-6`.

4. **The Prose Exception**:
   - The only acceptable use of margin is inside long-form prose and rich text (articles, blog posts, documentation flows, markdown rendering) where paragraphs, headings, blockquotes, and lists flow sequentially.
   - **CSS**: Apply margins only inside scoped prose selectors (e.g. `.prose p`, `.article h2`).
   - **Tailwind**: Use typography plugins or scoped selectors (e.g. `prose`, `prose-slate`).

5. **Use predefined scale values — no ad-hoc or arbitrary sizing**:
   - Always use defined design tokens, theme scales, or consistent spacing scales for widths, heights, padding, and gaps.
   - If a layout needs a new dimension, define a reusable token rather than inlining a one-off value.
   - **CSS**: Use custom properties from the token scale (e.g. `var(--space-4)`, `var(--size-container-md)`).
   - **Tailwind**: Use standard scale classes (e.g. `w-80`, `p-4`, `gap-6`, `max-w-xl`). Never use arbitrary bracket values such as `w-[320px]`, `p-[13px]`, or `gap-[15px]`.

6. **Strict Exception Policy**:
   - Only break these layout rules when strictly impossible to solve with flex, grid, gap, or padding.
   - When an exception is necessary, add a code comment explaining the constraint and why standard container spacing could not solve it.

---
name: john-ousterhout-software-design
type: reference
description: Reference for Ousterhout's A Philosophy of Software Design principles. ALWAYS read before designing modules, interfaces, or abstractions, or deciding how to split or combine code.
---

## A Philosophy of Software Design (Ousterhout)

Design principles from John Ousterhout's A Philosophy of Software Design. The developer's primary job is managing complexity, not writing code.

### Core Premise

- **Complexity is the root problem**: Complexity is anything related to the structure of a software system that makes it hard to understand and modify. It manifests through three symptoms:
  1. **Change amplification**: A seemingly simple change requires code modifications in many different places.
  2. **Cognitive load**: How much a developer needs to know in order to complete a task.
  3. **Unknown unknowns**: It is not obvious which pieces of code must be modified to complete a task, or what information a developer must have to carry it out safely.
- **Complexity is incremental**: It accumulates through hundreds of small dependencies and obscurity choices, each of which seems harmless on its own. Resist complexity on every change.
- **Strategic vs tactical programming**: Invest up front in good design (~10–20% overhead) rather than tactical "get it working fast" code that accrues complexity debt.
- **Shorter is not simpler**: Fewer lines of code do not mean simpler software if they increase cognitive load. Simplicity is achieved by hiding complexity behind a clean interface, not by minimizing line count.

### Module Design

- **Deep modules**: Keep interfaces simple relative to the functionality delivered ($\text{depth} = \text{functionality} / \text{interface complexity}$). Shallow modules add interface overhead and cognitive load without hiding complexity.
- **Information hiding**: Hide design decisions, data structures, invariants, and implementation details inside a single module so they can change without affecting callers. Information leakage — where details of one module are encoded in another — is the primary driver of change amplification.
- **Pull complexity downward**: When complexity is unavoidable, trap it inside the module implementation rather than exposing configuration parameters, edge cases, or coordination steps to callers. It is more important for a module to have a simple interface than a simple implementation.
- **Better together OR better apart**: Combine pieces of code when they exhibit any of these four indicators:
  1. **They share information** (e.g., both depend on the syntax or format of the same data structure).
  2. **They are used together** (anyone using one almost always uses the other).
  3. **They overlap conceptually** (both fall under a single higher-level category).
  4. **One is hard to understand without the other**. Split modules only when their responsibilities serve genuinely unrelated purposes.
- **Separate general-purpose mechanism from special-purpose policy**: Somewhat general-purpose modules are deeper and cleaner than special-purpose ones. Push specialized UI or policy code up into higher layers and keep lower-level modules general-purpose in their interface while implementing only what current requirements need.
- **Different layer, different abstraction**: As control passes from one layer to another, the abstraction level must change. Pass-through methods and decorators that merely forward calls with the same signature signal a shallow layer and confusion of responsibility.
- **Eliminate pass-through variables**: Variables passed down through a long chain of intermediate methods that do not use them force every intermediate signature to know about their existence. Store shared state in an existing context/domain object rather than threading parameters through unrelated layers.

### Interface & Error Design

- **Deep interfaces**: A clean signature should hide internal coordination, buffering, retries, and representation details. Avoid exposing both a high-level convenience API and the raw low-level primitives in the same interface unless strictly necessary.
- **Define errors out of existence**: Exception-heavy interfaces are shallow and force callers to scatter error-handling logic. Design operations so edge conditions are normal outcomes rather than exceptions (e.g., `unset`/`delete` succeeds idempotently when the key is already absent; substring clamps out-of-range bounds).
- **Mask or aggregate exceptions**: Handle recoverable low-level errors (e.g., transient network retries) inside the module where they occur, or aggregate granular low-level exceptions into a single higher-level handler rather than catching them at every call site.

### Comments & Obvious Code

- **Comments capture the non-obvious**: Focus comments on _why_ (design decisions, invariants, units, ownership, constraints, and rejected alternatives) and high-level _what_ (at the interface boundary), never low-level _how_ (repeating the code mechanics).
- **Comment-then-code**: Write interface and data-structure comments first, during design, before writing implementation code. If a module or method is hard to document concisely without describing its internal implementation, the abstraction is shallow.
- **Write obvious code**: Code is obvious when a reader can quickly understand its behavior and invariants without deep tracing, and their first intuition is correct. Avoid non-obvious patterns such as implicit event-driven control flow, generic unlabeled containers (`Pair`/`Tuple` instead of named fields), and types/variables that violate reader expectations.

### Design Process

- **Design it twice**: For any nontrivial module or interface, formulate at least two radically different design alternatives and compare their trade-offs before writing implementation code.
- **Strategize modifications**: When modifying existing code, do not make the smallest tactical diff that patches the behavior in place if it degrades the design. Refactor first so the new requirement fits naturally, leaving the system as clean as if it had been designed with that requirement from the start.
- **Consistency**: Follow established naming, structural, and behavioral conventions across the codebase so readers can leverage existing mental models without re-learning context.

### Red Flags (Quick Scan)

- **Shallow module**: Interface is nearly as complex as the implementation it wraps.
- **Information leakage**: Both caller and callee (or two sibling modules) encode knowledge of the same file format, protocol detail, or invariant.
- **Temporal decomposition**: Code structured around the chronological order in which operations happen (e.g., separate `read`, `process`, `write` classes that all share knowledge of the data layout) rather than around the information each module hides.
- **Overexposure**: An API forces callers to be aware of rarely used features or internal tuning parameters just to perform the common case.
- **Pass-through method / variable**: A method that only delegates to another method with a similar signature, or a variable threaded through intermediate functions that do not use it.
- **Repetition**: The same nontrivial code pattern repeated in multiple places because a deeper abstraction was not extracted.
- **Special-general mixture**: General-purpose mechanism code polluted with special-purpose caller assumptions or UI logic.
- **Conjoined methods**: Two methods (or a method and its caller) that are so tightly intertwined you cannot understand how one works without reading the implementation of the other.
- **Comment repeats code**: A comment that restates what is already obvious from the identifier names and syntax.
- **Vague name**: An identifier (`data`, `result`, `handle`, `manager`, `info`) too broad to convey what it represents or excludes.

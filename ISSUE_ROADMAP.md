# FlowRun issue analysis and roadmap

Reassessed: 2026-10-16, after committing the debug zoom fix. Scope: the **4 remaining unimplemented open issues** in [sacode387/FlowRun](https://github.com/sacode387/FlowRun/issues), plus the compact if/else layout idea. Closed issues and completed work are excluded. GitHub currently lists five open issues, including the implemented zoom fix awaiting closure; no open pull requests were returned.

This is a planning assessment based on current issue bodies/comments and source inspection. Estimates include implementation and focused verification for one developer familiar with the project; they are ranges, not commitments. The remaining features have not been implemented or validated at runtime.

## Recommendation

Implement **#32 (duplicate function)** next: it is the clearest remaining quick win. Prioritize **#14 (undo/redo)** as the next substantial feature because it improves everyday editing and recovery from mistakes. Give the **compact choice layout** a short visual prototype, then decide whether the readability gain justifies full implementation. Follow with **#6 (breakpoints)**. Clarify **#18 (generated code testing)** before scheduling it: a one-language CI check is a bounded quality task, while an interactive multi-language runner is a separate product investment.

There is no evidence from this review of a release-blocking defect requiring P0 treatment. Priorities reflect expected user benefit, implementation scope, and uncertainty rather than measured demand. Titles alone provide little scope for #14 and #32.

## Priorities and effort

P1 = next work; P2 = useful follow-up; P3 = defer or clarify. Effort is separate from value. A day means roughly one focused working day. Delivery rank refers to full implementation; the layout prototype can happen earlier within its explicit time box.

| Delivery rank | Item | Priority | Effort | Confidence | Reason |
| --- | --- | --- | --- | --- | --- |
| 1 | [#32 Duplicate function button](https://github.com/sacode387/FlowRun/issues/32) | P1 | 1–2 days | Medium–high | Existing statement cloning and function insertion provide most of the foundation. |
| 2 | [#14 Undo / redo](https://github.com/sacode387/FlowRun/issues/14) | P1 | 3–6 days | Medium | Broad daily benefit, with import, revision, selection, and event-handling edge cases. |
| 3 | New: compact if/else choice layout | P2 | 0.5–1 day prototype; 3–6 days total for a bounded first version | Medium–low | Promising readability improvement; layout, hit targets, and geometry require visual proof. |
| 4 | [#6 Debug point statement](https://github.com/sacode387/FlowRun/issues/6) | P2 | 2–4 days for temporary breakpoint markers | Medium–low | Step-by-step exists; stop-at-selected-statement remains. A persisted statement type is a larger scope. |
| 5 | [#18 Test generated code](https://github.com/sacode387/FlowRun/issues/18) | P3 pending scope | 2–4 days for one-language CI execution checks; multi-language coverage or an interactive runner is substantially larger | Low | The issue does not distinguish automated generator verification from executing generated code in the product. |

By implementation effort, **#32 is easiest**, followed by **temporary breakpoints** or a **narrow one-language generator check**. Undo/redo and full choice-lane layout are larger changes. The layout prototype is the cheapest way to reduce uncertainty, but is not a completed feature.

## Findings by issue

### #32 — Duplicate function

**Observed in source:** `Statement.duplicated` recursively clones blocks and control flow; `ProgramModel.addFunction(fun)` inserts a function, selects it, increments revision, and emits the existing update events. `FunctionSelector` already renders per-function actions.

**Proposed scope:** add Duplicate beside each editable non-main function. Copy its signature, parameters, and body; choose an unused name such as `calculate_copy` / `calculate_copy2`; select the copy. Give the function, parameters, statements, and blocks fresh IDs.

**Important detail:** `Statement.Begin.duplicated` returns the original statement. Blindly mapping `.duplicated` over a function body therefore retains the Begin ID. Function duplication needs to account for that. `Function.id` is derived from `rawId`, so construct a fresh raw ID instead of copying the already prefixed ID.

**First-version behavior:** retain call expressions as written, including calls to the source function. Renaming self-recursive calls is a separate semantic transformation; document this behavior rather than using string replacement. Exclude main from the initial UI scope because it has special identity and execution semantics.

**Acceptance:** repeated duplication produces unique names and IDs; nested branches/loops are independent; modifying or deleting a copy does not affect its source; JSON round-trip and execution work; read-only mode offers no duplication action.

### #14 — Undo / redo

**Observed in source:** AST values are case classes and are replaced through copies, making program snapshots a reasonable starting point. `ProgramModel` owns most changes, but exposes `var ast`, and file loading / source pasting assign it directly. Function selection also calls `doOnChange()`, so editor callbacks are not equivalent to model mutations.

**Proposed scope:** bounded in-memory program history, Undo/Redo buttons, and standard shortcuts. Treat statement/function changes, program rename, and configuration changes as edits; coalesce continuous typing. Function or statement selection, execution output, and debugger state should not create history entries. For full program import/paste, explicitly choose one behavior; recommended: one undoable replacement, including program identity, with selection reset/restored to valid IDs.

**Main work:** capture each logical edit once; clear redo after a new edit; restore selection and all affected UI; preserve monotonic revision/change notifications for host autosave; avoid replay recording itself as another edit. Disable history actions during execution. Respect native undo in focused text fields and support Cmd on macOS.

**Acceptance:** undo/redo add/edit/delete for nested statements and functions, rename/configuration, coalesced typing, and imports; a new edit after Undo invalidates Redo; deleting then restoring the selected function leaves valid selection; saved JSON and generated code match the restored state.

**Why not a quick win:** a stack is easy; consistently recording all mutation paths and notifying the embedding app is the real feature. Reuse snapshots before introducing a command framework.

### #6 — Debug point statement

**Observed in source:** the issue's comment also asks for step-by-step execution, which is present. `ExecMode.DEBUG` exists, but `waitForContinue()` only treats `STEP_BY_STEP` specially; DEBUG currently follows the ordinary continuation path. No debug-point AST statement or breakpoint UI was found.

**Proposed first version:** temporary breakpoint markers attached to existing statement IDs, with Continue and Step using the existing execution boundary. Pause before the marked statement, show variables/highlighting, and allow Continue to execute it once before rearming the breakpoint for later loop visits.

**Scope decision:** markers are a proposed interpretation of the original “stop execution here” request. If the desired feature is literally a new persisted DebugPoint statement, add schema/serialization compatibility, editing, rendering, and handling across the interpreter and code generators; estimate roughly 4–7 days instead.

**Acceptance:** pauses occur in loops and called functions; Continue does not immediately pause at the same boundary; Step advances once; Stop works while paused; deleting or importing a program does not leave stale breakpoint IDs. Reuse the existing statement pause boundary for breakpoint checks.

### #18 — Test generated code

**Observed in source:** the issue contains only a GraalVM polyglot link and “Jdoodle?”. There are **11 selectable target languages** in `Language` / `CodeGeneratorFactory`. Additional generator files do not imply supported selectable targets. Existing shared tests cover the interpreter, predefined functions, JSON, and lexer; no dedicated generator test suite was found.

**Recommended first scope:** automated checks in CI. Start with one commonly used target and a small set of programs: arithmetic, branches, loops, function arguments/returns, arrays, and scripted input/output. Generate source, compile where applicable, execute with bounded resources/time, and compare observable results with explicit expected outputs. Interpreter comparison can help diagnose disagreements but should not be the sole correctness oracle.

Golden source files can check formatting, but alone do not establish that generated code compiles or behaves correctly. Add other targets incrementally as toolchains and semantic differences are understood.

**Separate product decision:** an in-app Run Generated Code button requires runtime/service selection, support for input and output, execution limits, deployment work, and potentially external-service costs and data transfer. The issue's two references do not establish a viable solution. No current GraalVM or JDoodle compatibility/pricing research was done, so this roadmap makes no provider recommendation.

## New idea — Compact if/else “lane of choices”

### Interpretation

When a true or false branch contains **exactly one statement and that statement is an `If`**, display its condition inline with its parent as a lane of connected choices. The normal case is an `else if` chain; support the mirrored true-branch case as well. This interpretation is provisional: the original wording could also refer to a simple `if` with an empty opposite branch, which is a related but separate layout improvement.

For a false-branch chain, the intended visual is approximately:

```text
 [condition A?] --false--> [condition B?] --false--> [condition C?]
       | true                   | true                  | true    | false
       v                        v                       v         v
   [choice A]               [choice B]              [choice C] [fallback]
       |                        |                       |         |
       +------------------------+-----------------------+---------+
                                    |
                                  [next]
```

The diagram expresses evaluation order and alignment; exact connector routing and merge placement belong to the prototype. Each next condition is reached only through the labeled continuation edge. A true-branch chain uses a true continuation and false exits. Conditions must remain explicit: a lane must not look like independent conditions evaluated in parallel.

### Why this fits, and what makes it harder

`DotGenerator` already chooses explicit positions using `pos(...)`, branch extents through `widthTrue/widthFalse`, and dummy nodes for routing. `FlowchartPresenter` renders with the `neato` engine. This is a targeted layout change in the existing renderer; it does not require a new graph library.

However, each current `If` reserves its own branch space and merge node. A chain needs coordinated condition placement, branch widths, vertical extents, and return connectors. Node dimensions depend on label length. Compacting positions without updating width calculations and routes can overlap surrounding branches or loops.

Edges also carry editing information in `afterId@blockId` IDs, consumed by `CtxMenu.setEdgeIds()`. A shared visual merge must retain a correct insertion target for every editable edge. Preserving debugger node IDs is equally important.

### Bounded first version

- Apply the compact layout automatically to straight false-only or true-only chains of at least two conditions. Keep the existing AST, JSON, interpreter, and code generation unchanged.
- Require exactly one nested `If` in the continuing branch. Do not compact a branch containing assignments, comments, or other statements before or after that `If`; those statements carry order and need visible space.
- Initially retain ordinary layout when both branches qualify, continuation direction alternates, or surrounding geometry cannot be routed clearly. Show explicit branch labels in all layouts.
- Preserve statement IDs and block-aware insertion edges. Adding a second statement to a formerly qualifying branch automatically restores ordinary layout.
- Keep separate branch merge points if removing them makes edge insertion ambiguous. The visual prototype should establish whether a shared final merge is practical before it becomes a requirement.

### Prototype and acceptance

Time-box the prototype to 0.5–1 day: render a three-condition false chain and its true-chain mirror in the existing demo. Compare chart width/height, label legibility, and connector crossings against the current layout. A horizontal lane trades depth for width; include a narrow viewport rather than assuming that compact means smaller in both dimensions.

Full acceptance should include short/long conditions, empty outcomes, long bodies, nested loops, a chain inside a branch, and neighboring statements. Every condition must remain editable and highlightable; insertion on each branch and after the chain must affect the intended block; structural edits must switch layouts without changing program behavior. Use rendered visual checks plus focused tests of branch/edge mapping, rather than only snapshots of DOT strings.

**Priority:** P2 with high potential for teaching and readability. Time-box the prototype after function duplication; schedule the full feature after undo/redo unless the prototype proves especially small and useful. The estimate covers this restricted first version, not arbitrary flattening of nested decision trees.

## Suggested delivery sequence

1. **Function duplication:** #32, with fresh IDs throughout the copied function and nested statements.
2. **Bounded layout experiment:** spend at most 0.5–1 day on choice-lane examples and decide whether to proceed. Do not let an unresolved rendering experiment delay editing recovery.
3. **Editing recovery:** #14 in a dedicated feature change, covering every mutation path and host autosave notifications.
4. **Choice-lane implementation:** proceed if the prototype demonstrates a useful readability improvement without compromising insertion targets or execution highlighting.
5. **Advanced debugging:** #6, reusing existing stepping boundaries. It has no dependency on a timed execution feature.
6. **Generator quality:** clarify #18, then begin one-language CI checks. Raise this priority if a concrete wrong-code report appears.

The breakpoint issue is partially covered by existing step-by-step execution, but its original stop-at-selected-statement request remains unimplemented.

## Evidence and limits

- Refreshed issue bodies/comments using `gh issue list --state open --limit 200`; five issues were returned, four of which remain unimplemented. Closed states for the removed issues were separately confirmed. The completed zoom item is excluded even though it is still open on GitHub. GitHub itself was not modified.
- Current local FlowRun base: `de7f4fb` (debug zoom fix). That change passed editor compilation, demo linking, and local headless Chromium checks for navigation during stepping/input waits, edit locking, function transitions, and editing restoration after stop/completion. Those checks establish completed work; they do not validate the remaining features.
- Original application-source inspection used local revision `51abd46bbf985a0a321f60e55ec8d953e9907c3c`, one build-plugin-only commit ahead of remote `master` at `8dbe0f9eb47199e603d881d441a03aff3ec916b7`. The zoom fix does not change the AST, mutation paths, interpreter pause logic, code generators, or layout rules on which the remaining estimates rely.
- Relevant local sources:
  - [Program mutations](interpreter/shared/src/main/scala/dev/sacode/flowrun/ProgramModel.scala), [function model](interpreter/shared/src/main/scala/dev/sacode/flowrun/ast/program.scala), [statement cloning](interpreter/shared/src/main/scala/dev/sacode/flowrun/ast/statement.scala), [function actions](editor/src/main/scala/dev/sacode/flowrun/edit/FunctionsSelector.scala).
  - [Editor events and controls](editor/src/main/scala/dev/sacode/flowrun/FlowRunEditor.scala), [interpreter pause logic](interpreter/shared/src/main/scala/dev/sacode/flowrun/eval/Interpreter.scala), [layout generator](interpreter/shared/src/main/scala/dev/sacode/flowrun/formatgen/DotGenerator.scala), [edge editing](editor/src/main/scala/dev/sacode/flowrun/edit/CtxMenu.scala), [supported target languages](interpreter/shared/src/main/scala/dev/sacode/flowrun/codegen/Language.scala).
- Priorities and estimates are judgments from this bounded review. Rendering prototypes, implementation-specific timing, and external execution-provider feasibility remain future work.

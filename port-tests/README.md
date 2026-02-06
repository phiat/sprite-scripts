# Port Tests

End-to-end tests of sprite-tool ports. Each test launches a real sprite on [sprites.dev](https://sprites.dev), pushes a plan file, and has a coding agent execute it autonomously.

## Agent Harness

- **Agent**: [OpenCode](https://opencode.ai) (`opencode run`)
- **Model**: `opencode/big-pickle` (free tier)
- **Invocation**: Each port's `sprite-tool launch <sprite-name> <plan-file>` command handles the full lifecycle:
  1. Authenticate with sprites.dev API
  2. Create a new sprite
  3. Push `.env` and plan file to the sprite
  4. Initialize git and install [beads](https://github.com/steveyegge/beads) issue tracker
  5. Install OpenCode
  6. Run `opencode run -m opencode/big-pickle 'read plan.md and complete the plan please'`
  7. Auto-checkpoint every 300s during execution
  8. Final checkpoint on completion

## Plan Template

Each test uses a language-specific poem plan. Example (`c-poem-plan.md`):

```markdown
# Plan: Write Three Poems About C

Write three short poems about the **C programming language** and save them as text files.

## Requirements

1. **poem-1.txt** — A haiku-style poem (4-8 lines) about C
2. **poem-2.txt** — A limerick (2 stanzas) about C's quirks (pointers, manual memory, segfaults, etc.)
3. **poem-3.txt** — A free-verse ode (8-12 lines) celebrating what makes C unique

Each poem must specifically reference C, its features, history, or ecosystem.

## Completion

When all three poems are written, create a file called **done.txt** listing what was created.
```

## Test Results

| Port | Sprite Name | Poems Written | Status |
|------|-------------|---------------|--------|
| Deno/TypeScript | deno-poems | 3/3 | Pass |
| Zig | zig-poems | 3/3 | Pass |
| C | c-poems | 3/3 | Pass |
| C++ | cpp-poems | 3/3 | Pass |
| C# | csharp-poems | 3/3 | Pass |
| F# | fsharp-poems | 3/3 | Pass |

## Directory Structure

```
port-tests/
├── README.md
├── c-poem-plan.md
├── cpp-poem-plan.md
├── csharp-poem-plan.md
├── deno-poem-plan.md
├── fsharp-poem-plan.md
├── zig-poem-plan.md
├── c-poems/
│   ├── poem-1.txt
│   ├── poem-2.txt
│   └── poem-3.txt
├── cpp-poems/
│   ├── poem-1.txt
│   ├── poem-2.txt
│   └── poem-3.txt
├── csharp-poems/
│   ├── poem-1.txt
│   ├── poem-2.txt
│   └── poem-3.txt
├── deno-poems/
│   ├── poem-1.txt
│   ├── poem-2.txt
│   └── poem-3.txt
├── fsharp-poems/
│   ├── poem-1.txt
│   ├── poem-2.txt
│   └── poem-3.txt
└── zig-poems/
    ├── poem-1.txt
    ├── poem-2.txt
    └── poem-3.txt
```

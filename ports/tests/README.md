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

Each test uses a language-specific poem plan generated from this template (replacing `{LANGUAGE}`):

```markdown
# Plan: Write Three Poems About {LANGUAGE}

Write three short poems about the **{LANGUAGE} programming language** and save them as text files.

## Requirements

1. **poem-1.txt** — A haiku-style poem (4-8 lines) about {LANGUAGE}
2. **poem-2.txt** — A limerick (2 stanzas) about {LANGUAGE}'s quirks
3. **poem-3.txt** — A free-verse ode (8-12 lines) celebrating what makes {LANGUAGE} unique

Each poem must specifically reference {LANGUAGE}, its features, history, or ecosystem.

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
| Go | go-poems | 3/3 | Pass |
| Rust | rust-poems | 3/3 | Pass |
| Python | python-poems | 3/3 | Pass |
| Ruby | ruby-poems | 3/3 | Pass |

## Directory Structure

```
ports/tests/
├── README.md
├── c-poems/
├── cpp-poems/
├── csharp-poems/
├── deno-poems/
├── fsharp-poems/
├── go-poems/
├── python-poems/
├── ruby-poems/
├── rust-poems/
└── zig-poems/
    ├── poem-1.txt        # Each contains 3 poems
    ├── poem-2.txt
    └── poem-3.txt
```

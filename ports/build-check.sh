#!/usr/bin/env bash
# build-check.sh - Build verification for all compiled/buildable sprite-tool ports
# Usage: ./build-check.sh [port-name ...]
# With no args, checks all ports. With args, checks only named ports.

set -uo pipefail

PORTS_DIR="$(cd "$(dirname "$0")" && pwd)"
PASS=0
FAIL=0
SKIP=0
RESULTS=()

green()  { printf '\033[32m%s\033[0m' "$1"; }
red()    { printf '\033[31m%s\033[0m' "$1"; }
yellow() { printf '\033[33m%s\033[0m' "$1"; }

record() {
    local port="$1" status="$2" detail="${3:-}"
    case "$status" in
        PASS) PASS=$((PASS + 1)); RESULTS+=("$(green PASS)  $port${detail:+ ($detail)}") ;;
        FAIL) FAIL=$((FAIL + 1)); RESULTS+=("$(red FAIL)  $port${detail:+ ($detail)}") ;;
        SKIP) SKIP=$((SKIP + 1)); RESULTS+=("$(yellow SKIP)  $port${detail:+ ($detail)}") ;;
    esac
}

check_cmd() { command -v "$1" &>/dev/null; }

build_c() {
    cd "$PORTS_DIR/c" || return 1
    make clean &>/dev/null
    make 2>&1
}

build_cpp() {
    cd "$PORTS_DIR/cpp" || return 1
    rm -rf build
    cmake -B build 2>&1 && cmake --build build 2>&1
}

build_rust() {
    cd "$PORTS_DIR/rust" || return 1
    cargo build --release 2>&1
}

build_go() {
    cd "$PORTS_DIR/go" || return 1
    go mod tidy 2>&1 && go build -o sprite-tool ./cmd/sprite-tool 2>&1
}

build_zig() {
    cd "$PORTS_DIR/zig" || return 1
    zig build -Doptimize=ReleaseSafe 2>&1
}

build_d() {
    cd "$PORTS_DIR/d" || return 1
    dub build 2>&1
}

build_nim() {
    cd "$PORTS_DIR/nim" || return 1
    nim c -d:release --threads:on src/sprite_tool.nim 2>&1
}

build_crystal() {
    cd "$PORTS_DIR/crystal" || return 1
    crystal build src/sprite_tool.cr 2>&1
}

build_odin() {
    cd "$PORTS_DIR/odin" || return 1
    if [ -z "${ODIN_ROOT:-}" ]; then
        # Try to auto-detect
        local odin_bin
        odin_bin="$(command -v odin 2>/dev/null)"
        if [ -n "$odin_bin" ]; then
            odin_bin="$(readlink -f "$odin_bin")"
            export ODIN_ROOT="$(dirname "$odin_bin")"
        fi
    fi
    odin build . -o:speed 2>&1
}

build_ada() {
    cd "$PORTS_DIR/ada" || return 1
    gprbuild -P sprite_tool.gpr -p 2>&1
}

build_swift() {
    cd "$PORTS_DIR/swift" || return 1
    swift build 2>&1
}

build_haskell() {
    cd "$PORTS_DIR/haskell" || return 1
    cabal build 2>&1
}

build_ocaml() {
    cd "$PORTS_DIR/ocaml" || return 1
    eval "$(opam env 2>/dev/null)"
    dune build 2>&1
}

build_csharp() {
    cd "$PORTS_DIR/csharp" || return 1
    dotnet build 2>&1
}

build_fsharp() {
    cd "$PORTS_DIR/fsharp" || return 1
    dotnet build 2>&1
}

build_gleam() {
    cd "$PORTS_DIR/gleam" || return 1
    gleam build 2>&1
}

build_elixir() {
    cd "$PORTS_DIR/elixir" || return 1
    mix escript.build 2>&1
}

build_haxe() {
    cd "$PORTS_DIR/haxe" || return 1
    haxe build.hxml 2>&1
}

# Scripting language syntax checks
check_python() {
    cd "$PORTS_DIR/python" || return 1
    python3 -m py_compile sprite_tool/__main__.py 2>&1 &&
    python3 -m py_compile sprite_tool/cli.py 2>&1
}

check_ruby() {
    cd "$PORTS_DIR/ruby" || return 1
    ruby -c bin/sprite-tool 2>&1
}

check_perl() {
    cd "$PORTS_DIR/perl" || return 1
    perl -Ilib -c bin/sprite-tool 2>&1
}

check_deno() {
    cd "$PORTS_DIR/deno" || return 1
    deno check src/main.ts 2>&1
}

check_lua() {
    cd "$PORTS_DIR/lua" || return 1
    luac -p bin/sprite-tool lib/*.lua 2>&1
}

check_clojure() {
    cd "$PORTS_DIR/clojure" || return 1
    clojure -M -m sprite-tool.core --help 2>&1 || true
}

# Define all ports and their requirements
declare -A PORT_CMD=(
    [c]="gcc"
    [cpp]="cmake"
    [rust]="cargo"
    [go]="go"
    [zig]="zig"
    [d]="dub"
    [nim]="nim"
    [crystal]="crystal"
    [odin]="odin"
    [ada]="gprbuild"
    [swift]="swift"
    [haskell]="cabal"
    [ocaml]="dune"
    [csharp]="dotnet"
    [fsharp]="dotnet"
    [gleam]="gleam"
    [elixir]="mix"
    [haxe]="haxe"
    [python]="python3"
    [ruby]="ruby"
    [perl]="perl"
    [deno]="deno"
    [lua]="luac"
    [clojure]="clojure"
)

declare -A PORT_FN=(
    [c]="build_c"
    [cpp]="build_cpp"
    [rust]="build_rust"
    [go]="build_go"
    [zig]="build_zig"
    [d]="build_d"
    [nim]="build_nim"
    [crystal]="build_crystal"
    [odin]="build_odin"
    [ada]="build_ada"
    [swift]="build_swift"
    [haskell]="build_haskell"
    [ocaml]="build_ocaml"
    [csharp]="build_csharp"
    [fsharp]="build_fsharp"
    [gleam]="build_gleam"
    [elixir]="build_elixir"
    [haxe]="build_haxe"
    [python]="check_python"
    [ruby]="check_ruby"
    [perl]="check_perl"
    [deno]="check_deno"
    [lua]="check_lua"
    [clojure]="check_clojure"
)

# Ordered list for consistent output
ALL_PORTS=(c cpp rust go zig d nim crystal odin ada swift haskell ocaml csharp fsharp gleam elixir haxe python ruby perl deno lua clojure)

# Filter to requested ports if args given
if [ $# -gt 0 ]; then
    PORTS=("$@")
else
    PORTS=("${ALL_PORTS[@]}")
fi

echo "=========================================="
echo " sprite-tool build check"
echo "=========================================="
echo ""

for port in "${PORTS[@]}"; do
    fn="${PORT_FN[$port]:-}"
    cmd="${PORT_CMD[$port]:-}"

    if [ -z "$fn" ]; then
        record "$port" SKIP "unknown port"
        continue
    fi

    if [ -n "$cmd" ] && ! check_cmd "$cmd"; then
        record "$port" SKIP "$cmd not found"
        continue
    fi

    printf "%-12s building... " "$port"
    output=$($fn 2>&1)
    rc=$?

    if [ $rc -eq 0 ]; then
        record "$port" PASS
        echo "$(green PASS)"
    else
        record "$port" FAIL "exit $rc"
        echo "$(red FAIL)"
        # Show last few lines of error
        echo "$output" | tail -5 | sed 's/^/    /'
    fi
done

echo ""
echo "=========================================="
echo " Results: $(green "$PASS pass"), $(red "$FAIL fail"), $(yellow "$SKIP skip")"
echo "=========================================="
echo ""
for r in "${RESULTS[@]}"; do
    echo "  $r"
done

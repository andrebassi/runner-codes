#!/bin/bash
# =============================================================================
# LLM-FireSandbox: Test All Languages Execution
# =============================================================================
# Tests real code execution for all 40 languages with timeout protection.
#
# Usage:
#   ./test-executions.sh           # Test all languages
#   ./test-executions.sh python go # Test specific languages
#
# =============================================================================

set -e

IMAGES_DIR="/srv/firecracker/images"
RESULTS_FILE="/tmp/test-results-$(date +%Y%m%d-%H%M%S).txt"
TIMEOUT=30  # seconds per test

echo "=============================================="
echo "LLM-FireSandbox - Execution Tests"
echo "Started: $(date)"
echo "Results: $RESULTS_FILE"
echo "=============================================="
echo ""

> $RESULTS_FILE

PASSED=0
FAILED=0
TOTAL=0

test_lang() {
    local LANG=$1
    local FILENAME=$2
    local CODE=$3
    local RUN_CMD=$4

    local ROOTFS="${IMAGES_DIR}/rootfs-${LANG}.ext4"

    ((TOTAL++))

    if [ ! -f "$ROOTFS" ]; then
        echo "[$LANG] SKIP - rootfs not found"
        echo "$LANG:SKIP:rootfs not found" >> $RESULTS_FILE
        ((FAILED++))
        return
    fi

    echo -n "[$LANG] Testing... "

    # Cleanup any previous mount
    sudo umount /mnt/test-${LANG} 2>/dev/null || true
    sudo rm -rf /mnt/test-${LANG}

    # Mount rootfs
    sudo mkdir -p /mnt/test-${LANG}
    if ! sudo mount -o loop "$ROOTFS" /mnt/test-${LANG} 2>/dev/null; then
        echo "FAIL (mount error)"
        echo "$LANG:FAIL:mount error" >> $RESULTS_FILE
        ((FAILED++))
        sudo rm -rf /mnt/test-${LANG}
        return
    fi

    # Mount /proc for languages that need it
    sudo mount -t proc proc /mnt/test-${LANG}/proc 2>/dev/null || true

    # Create test file
    sudo mkdir -p /mnt/test-${LANG}/tmp/test
    echo "$CODE" | sudo tee /mnt/test-${LANG}/tmp/test/${FILENAME} > /dev/null

    # Execute with timeout
    local OUTPUT=""
    OUTPUT=$(timeout ${TIMEOUT}s sudo chroot /mnt/test-${LANG} /bin/sh -c "cd /tmp/test && $RUN_CMD" 2>&1) || true

    # Cleanup
    sudo rm -rf /mnt/test-${LANG}/tmp/test
    sudo umount /mnt/test-${LANG}/proc 2>/dev/null || true
    sudo umount /mnt/test-${LANG} 2>/dev/null || true
    sudo rm -rf /mnt/test-${LANG}

    # Check output
    if echo "$OUTPUT" | grep -q "Hello World"; then
        echo "OK"
        echo "$LANG:OK" >> $RESULTS_FILE
        ((PASSED++))
    else
        local SHORT_OUT=$(echo "$OUTPUT" | head -c 100 | tr '\n' ' ')
        echo "FAIL"
        echo "  Output: $SHORT_OUT"
        echo "$LANG:FAIL:$SHORT_OUT" >> $RESULTS_FILE
        ((FAILED++))
    fi
}

# =============================================================================
# ALL LANGUAGE TESTS
# =============================================================================

run_all_tests() {
    # Python
    test_lang "python" "test.py" \
        'print("Hello World")' \
        'python3 test.py'

    # Node.js
    test_lang "nodejs" "test.js" \
        'console.log("Hello World")' \
        'node test.js'

    # TypeScript
    test_lang "typescript" "test.ts" \
        'console.log("Hello World")' \
        'npx ts-node test.ts 2>/dev/null || node test.ts'

    # Go
    test_lang "go" "test.go" \
        'package main
import "fmt"
func main() { fmt.Println("Hello World") }' \
        'go run test.go'

    # Rust
    test_lang "rust" "test.rs" \
        'fn main() { println!("Hello World"); }' \
        'rustc test.rs -o test && ./test'

    # Bash
    test_lang "bash" "test.sh" \
        'echo "Hello World"' \
        'bash test.sh'

    # C
    test_lang "c" "test.c" \
        '#include <stdio.h>
int main() { printf("Hello World\n"); return 0; }' \
        'gcc test.c -o test && ./test'

    # C++
    test_lang "cpp" "test.cpp" \
        '#include <iostream>
int main() { std::cout << "Hello World" << std::endl; return 0; }' \
        'g++ test.cpp -o test && ./test'

    # Java
    test_lang "java" "Test.java" \
        'public class Test { public static void main(String[] args) { System.out.println("Hello World"); } }' \
        'javac Test.java && java Test'

    # Kotlin
    test_lang "kotlin" "test.kt" \
        'fun main() { println("Hello World") }' \
        'kotlinc test.kt -include-runtime -d test.jar 2>/dev/null && java -jar test.jar'

    # Scala
    test_lang "scala" "test.scala" \
        'object Test extends App { println("Hello World") }' \
        'scala test.scala 2>/dev/null'

    # Ruby
    test_lang "ruby" "test.rb" \
        'puts "Hello World"' \
        'ruby test.rb'

    # PHP
    test_lang "php" "test.php" \
        '<?php echo "Hello World\n"; ?>' \
        'php test.php'

    # Perl
    test_lang "perl" "test.pl" \
        'print "Hello World\n";' \
        'perl test.pl'

    # Lua
    test_lang "lua" "test.lua" \
        'print("Hello World")' \
        'lua5.4 test.lua 2>/dev/null || lua test.lua'

    # R
    test_lang "r" "test.R" \
        'cat("Hello World\n")' \
        'Rscript test.R 2>/dev/null'

    # Haskell
    test_lang "haskell" "test.hs" \
        'main = putStrLn "Hello World"' \
        'runghc test.hs 2>/dev/null'

    # Elixir
    test_lang "elixir" "test.exs" \
        'IO.puts "Hello World"' \
        'elixir test.exs'

    # Erlang
    test_lang "erlang" "test.erl" \
        '-module(test).
-export([main/0]).
main() -> io:format("Hello World~n").' \
        'erlc test.erl && erl -noshell -s test main -s init stop'

    # Julia
    test_lang "julia" "test.jl" \
        'println("Hello World")' \
        'julia test.jl'

    # Fortran
    test_lang "fortran" "test.f90" \
        'program hello
  print *, "Hello World"
end program hello' \
        'gfortran test.f90 -o test && ./test'

    # Lisp (SBCL)
    test_lang "lisp" "test.lisp" \
        '(format t "Hello World~%")' \
        'sbcl --script test.lisp'

    # Scheme (Guile)
    test_lang "scheme" "test.scm" \
        '(display "Hello World") (newline)' \
        'guile test.scm 2>/dev/null'

    # Prolog (SWI-Prolog)
    test_lang "prolog" "test.pl" \
        ':- initialization(main).
main :- write("Hello World"), nl, halt.' \
        'swipl -q -s test.pl'

    # Pascal
    test_lang "pascal" "test.pas" \
        "program Hello;
begin
  writeln('Hello World');
end." \
        'fpc test.pas -o test >/dev/null 2>&1 && ./test'

    # OCaml
    test_lang "ocaml" "test.ml" \
        'print_endline "Hello World"' \
        'ocaml test.ml'

    # Nim
    test_lang "nim" "test.nim" \
        'echo "Hello World"' \
        'nim c --verbosity:0 --hints:off test.nim 2>/dev/null && ./test'

    # Crystal
    test_lang "crystal" "test.cr" \
        'puts "Hello World"' \
        'crystal run test.cr 2>/dev/null'

    # D
    test_lang "d" "test.d" \
        'import std.stdio;
void main() { writeln("Hello World"); }' \
        'ldc2 test.d -of=test 2>/dev/null && ./test'

    # Zig
    test_lang "zig" "test.zig" \
        'const std = @import("std");
pub fn main() void {
    std.debug.print("Hello World\n", .{});
}' \
        'zig build-exe test.zig 2>/dev/null && ./test'

    # COBOL
    test_lang "cobol" "test.cob" \
        '       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "Hello World".
           STOP RUN.' \
        'cobc -x -free test.cob -o test 2>/dev/null && ./test'

    # Groovy
    test_lang "groovy" "test.groovy" \
        'println "Hello World"' \
        'groovy test.groovy'

    # Clojure
    test_lang "clojure" "test.clj" \
        '(println "Hello World")' \
        'clojure -M test.clj 2>/dev/null || java -jar /usr/local/lib/clojure/clojure.jar test.clj 2>/dev/null'

    # SQLite
    test_lang "sqlite" "test.sql" \
        'SELECT "Hello World";' \
        'sqlite3 :memory: < test.sql'

    # TCL
    test_lang "tcl" "test.tcl" \
        'puts "Hello World"' \
        'tclsh test.tcl'

    # AWK
    test_lang "awk" "test.awk" \
        'BEGIN { print "Hello World" }' \
        'awk -f test.awk'

    # JQ
    test_lang "jq" "test.jq" \
        '"Hello World"' \
        'jq -r -n -f test.jq'

    # NASM (x86_64 Assembly)
    test_lang "nasm" "test.asm" \
        'section .data
    msg db "Hello World", 10
    len equ $ - msg
section .text
    global _start
_start:
    mov rax, 1
    mov rdi, 1
    mov rsi, msg
    mov rdx, len
    syscall
    mov rax, 60
    xor rdi, rdi
    syscall' \
        'nasm -f elf64 test.asm && ld test.o -o test && ./test'

    # Octave
    test_lang "octave" "test.m" \
        'disp("Hello World")' \
        'octave --silent test.m'

    # .NET (C#)
    test_lang "dotnet" "Test.cs" \
        'using System;
class Test { static void Main() { Console.WriteLine("Hello World"); } }' \
        'mkdir -p testapp && cp Test.cs testapp/Program.cs && cd testapp && dotnet new console --force >/dev/null 2>&1 && dotnet run 2>/dev/null'
}

# =============================================================================
# MAIN
# =============================================================================

if [ $# -gt 0 ]; then
    # Test specific languages
    for lang in "$@"; do
        case $lang in
            python) test_lang "python" "test.py" 'print("Hello World")' 'python3 test.py' ;;
            nodejs) test_lang "nodejs" "test.js" 'console.log("Hello World")' 'node test.js' ;;
            go) test_lang "go" "test.go" 'package main
import "fmt"
func main() { fmt.Println("Hello World") }' 'go run test.go' ;;
            rust) test_lang "rust" "test.rs" 'fn main() { println!("Hello World"); }' 'rustc test.rs -o test && ./test' ;;
            *) echo "Unknown language: $lang" ;;
        esac
    done
else
    # Run all tests
    run_all_tests
fi

echo ""
echo "=============================================="
echo "RESULTS SUMMARY"
echo "=============================================="
echo ""
echo "PASSED: $PASSED"
echo "FAILED: $FAILED"
echo "TOTAL:  $TOTAL"
echo ""

if [ $FAILED -gt 0 ]; then
    echo "Failed languages:"
    grep ":FAIL:" $RESULTS_FILE | cut -d: -f1 | while read lang; do
        echo "  - $lang"
    done
fi

echo ""
echo "Completed: $(date)"
echo "Full results: $RESULTS_FILE"

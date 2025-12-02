---
title: 'Erlang'
description: 'Erlang concurrent programming with escript'
---

## Overview

Erlang is a functional language designed for concurrent, distributed systems. Runner Codes provides Erlang with escript for script execution.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `erlang:alpine` |
| Version | Erlang/OTP 28 |
| Rootfs Size | 400 MB |
| Execution | Interpreted (escript) |
| File Extension | `.erl` |
| Run Command | `escript {file}` |
| Execution Time | ~5.4s |

```bash title="1. Create Rootfs with infra.operator"
sudo infra.operator rootfs from-docker --name erlang --image erlang:alpine --size 400
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang erlang --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang erlang --bucket runner-codes
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang erlang --bucket runner-codes
```

```bash title="4. Test Execution"
sudo infra.operator host --lang erlang --code "#!/usr/bin/env escript
main(_) ->
    io:format(\"Hello from Erlang!~n\")." --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "erl-hello-001",
  "lang": "erlang",
  "code": "#!/usr/bin/env escript\nmain(_) ->\n    io:format(\"Hello from Erlang!~n\").",
  "timeout": 15
}
```

```json title="Response"
{
  "trace_id": "erl-hello-001",
  "stdout": "Hello from Erlang!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Concurrent Processing

```json title="Request"
{
  "trace_id": "erl-complex-001",
  "lang": "erlang",
  "code": "#!/usr/bin/env escript\n\n%% Fibonacci function\nfib(0) -> 0;\nfib(1) -> 1;\nfib(N) when N > 1 -> fib(N-1) + fib(N-2).\n\n%% QuickSort implementation\nquicksort([]) -> [];\nquicksort([Pivot|Rest]) ->\n    quicksort([X || X <- Rest, X < Pivot])\n    ++ [Pivot] ++\n    quicksort([X || X <- Rest, X >= Pivot]).\n\n%% Map function\nmy_map(_, []) -> [];\nmy_map(F, [H|T]) -> [F(H) | my_map(F, T)].\n\n%% Filter function\nmy_filter(_, []) -> [];\nmy_filter(Pred, [H|T]) ->\n    case Pred(H) of\n        true -> [H | my_filter(Pred, T)];\n        false -> my_filter(Pred, T)\n    end.\n\n%% Reduce/fold function\nmy_reduce(_, Acc, []) -> Acc;\nmy_reduce(F, Acc, [H|T]) -> my_reduce(F, F(Acc, H), T).\n\n%% Process simulation\nworker(Parent, Id, Value) ->\n    Result = Value * Value,\n    Parent ! {result, Id, Result}.\n\ncollect_results(0, Acc) -> Acc;\ncollect_results(N, Acc) ->\n    receive\n        {result, Id, Value} ->\n            collect_results(N-1, [{Id, Value} | Acc])\n    end.\n\nmain(_) ->\n    io:format(\"=== Erlang Complex Test ===~n~n\"),\n    \n    %% Test 1: Fibonacci\n    io:format(\"1. Fibonacci sequence:~n\"),\n    FibNums = [fib(N) || N <- lists:seq(0, 14)],\n    io:format(\"   First 15: ~p~n\", [FibNums]),\n    io:format(\"   Fib(25) = ~p~n\", [fib(25)]),\n    \n    %% Test 2: QuickSort\n    io:format(\"~n2. QuickSort:~n\"),\n    Unsorted = [64, 34, 25, 12, 22, 11, 90],\n    Sorted = quicksort(Unsorted),\n    io:format(\"   Input:  ~p~n\", [Unsorted]),\n    io:format(\"   Output: ~p~n\", [Sorted]),\n    \n    %% Test 3: Higher-order functions\n    io:format(\"~n3. Higher-order functions on [1..10]:~n\"),\n    Numbers = lists:seq(1, 10),\n    Squares = my_map(fun(X) -> X * X end, Numbers),\n    Evens = my_filter(fun(X) -> X rem 2 == 0 end, Numbers),\n    Sum = my_reduce(fun(Acc, X) -> Acc + X end, 0, Numbers),\n    io:format(\"   Squares: ~p~n\", [Squares]),\n    io:format(\"   Evens: ~p~n\", [Evens]),\n    io:format(\"   Sum: ~p~n\", [Sum]),\n    \n    %% Test 4: Pattern matching\n    io:format(\"~n4. Pattern matching:~n\"),\n    Tuples = [{alice, 30}, {bob, 25}, {charlie, 35}],\n    lists:foreach(\n        fun({Name, Age}) ->\n            io:format(\"   ~p is ~p years old~n\", [Name, Age])\n        end,\n        Tuples\n    ),\n    \n    %% Test 5: List comprehensions\n    io:format(\"~n5. List comprehensions:~n\"),\n    Pairs = [{X, Y} || X <- [1,2,3], Y <- [a,b]],\n    io:format(\"   Cartesian product: ~p~n\", [Pairs]),\n    \n    io:format(\"~n=== All tests passed ===~n\").",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "erl-complex-001",
  "stdout": "=== Erlang Complex Test ===\n\n1. Fibonacci sequence:\n   First 15: [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]\n   Fib(25) = 75025\n\n2. QuickSort:\n   Input:  [64,34,25,12,22,11,90]\n   Output: [11,12,22,25,34,64,90]\n\n3. Higher-order functions on [1..10]:\n   Squares: [1,4,9,16,25,36,49,64,81,100]\n   Evens: [2,4,6,8,10]\n   Sum: 55\n\n4. Pattern matching:\n   alice is 30 years old\n   bob is 25 years old\n   charlie is 35 years old\n\n5. List comprehensions:\n   Cartesian product: [{1,a},{1,b},{2,a},{2,b},{3,a},{3,b}]\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  - No OTP applications (escript only)
  - No external dependencies
  - Memory limit: 512 MiB

:::

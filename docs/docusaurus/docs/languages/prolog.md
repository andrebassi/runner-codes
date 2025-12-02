---
title: 'Prolog'
description: 'Prolog logic programming'
---

## Overview

Prolog is a logic programming language. Runner Codes provides SWI-Prolog for logic programming.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `debian:bookworm-slim` + swi-prolog |
| Version | SWI-Prolog 9.0.4 |
| Rootfs Size | 600 MB |
| Execution | Interpreted (swipl) |
| File Extension | `.pl` |
| Run Command | `swipl -q -t halt -s {file}` |
| Execution Time | ~26ms |

```bash title="Build: FROM debian:bookworm-slim + apt-get install swi-prolog"
sudo infra.operator rootfs from-docker --name prolog --image prolog:custom --size 600
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang prolog --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang prolog --bucket runner-codes
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang prolog --bucket runner-codes
```

```bash title="4. Test Execution"
sudo infra.operator host --lang prolog --code ':- initialization(main, main).
main :- write("Hello from Prolog!"), nl, halt.' --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "pl-hello-001",
  "lang": "prolog",
  "code": ":- initialization(main, main).\n\nmain :- write('Hello from Prolog!'), nl, halt.",
  "timeout": 15
}
```

```json title="Response"
{
  "trace_id": "pl-hello-001",
  "stdout": "Hello from Prolog!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Logic and Knowledge Base

```json title="Request"
{
  "trace_id": "pl-complex-001",
  "lang": "prolog",
  "code": ":- initialization(main, main).\n\n%% Knowledge base - Family relationships\nparent(tom, bob).\nparent(tom, liz).\nparent(bob, ann).\nparent(bob, pat).\nparent(pat, jim).\n\nmale(tom).\nmale(bob).\nmale(jim).\nfemale(liz).\nfemale(ann).\nfemale(pat).\n\n%% Rules\nfather(X, Y) :- parent(X, Y), male(X).\nmother(X, Y) :- parent(X, Y), female(X).\ngrandparent(X, Z) :- parent(X, Y), parent(Y, Z).\nancestor(X, Y) :- parent(X, Y).\nancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).\nsibling(X, Y) :- parent(P, X), parent(P, Y), X \\= Y.\n\n%% Fibonacci\nfib(0, 0) :- !.\nfib(1, 1) :- !.\nfib(N, F) :-\n    N > 1,\n    N1 is N - 1,\n    N2 is N - 2,\n    fib(N1, F1),\n    fib(N2, F2),\n    F is F1 + F2.\n\n%% List operations\nmy_length([], 0).\nmy_length([_|T], N) :- my_length(T, N1), N is N1 + 1.\n\nmy_sum([], 0).\nmy_sum([H|T], S) :- my_sum(T, S1), S is H + S1.\n\nmy_reverse([], []).\nmy_reverse([H|T], R) :- my_reverse(T, RT), append(RT, [H], R).\n\nquicksort([], []).\nquicksort([H|T], Sorted) :-\n    partition(H, T, Less, Greater),\n    quicksort(Less, SortedLess),\n    quicksort(Greater, SortedGreater),\n    append(SortedLess, [H|SortedGreater], Sorted).\n\npartition(_, [], [], []).\npartition(Pivot, [H|T], [H|Less], Greater) :-\n    H =< Pivot, !,\n    partition(Pivot, T, Less, Greater).\npartition(Pivot, [H|T], Less, [H|Greater]) :-\n    partition(Pivot, T, Less, Greater).\n\n%% Main program\nmain :-\n    write('=== Prolog Complex Test ==='), nl, nl,\n    \n    %% Test 1: Family queries\n    write('1. Family relationships:'), nl,\n    (father(tom, bob) -> write('   Tom is Bob\\'s father') ; true), nl,\n    findall(X, grandparent(tom, X), Grandchildren),\n    write('   Tom\\'s grandchildren: '), write(Grandchildren), nl,\n    findall(X, ancestor(tom, X), Descendants),\n    write('   Tom\\'s descendants: '), write(Descendants), nl,\n    \n    %% Test 2: Fibonacci\n    nl, write('2. Fibonacci sequence:'), nl,\n    write('   First 10: '),\n    forall(between(0, 9, N), (fib(N, F), write(F), write(' '))), nl,\n    fib(20, F20),\n    write('   Fib(20) = '), write(F20), nl,\n    \n    %% Test 3: List operations\n    nl, write('3. List operations:'), nl,\n    Numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],\n    my_length(Numbers, Len),\n    write('   Length of '), write(Numbers), write(': '), write(Len), nl,\n    my_sum(Numbers, Sum),\n    write('   Sum: '), write(Sum), nl,\n    my_reverse(Numbers, Rev),\n    write('   Reversed: '), write(Rev), nl,\n    \n    %% Test 4: QuickSort\n    nl, write('4. QuickSort:'), nl,\n    Unsorted = [64, 34, 25, 12, 22, 11, 90],\n    write('   Input:  '), write(Unsorted), nl,\n    quicksort(Unsorted, Sorted),\n    write('   Output: '), write(Sorted), nl,\n    \n    %% Test 5: Pattern matching\n    nl, write('5. Pattern matching:'), nl,\n    findall(X, sibling(X, ann), Siblings),\n    write('   Ann\\'s siblings: '), write(Siblings), nl,\n    findall(X, male(X), Males),\n    write('   All males: '), write(Males), nl,\n    \n    nl, write('=== All tests passed ==='), nl,\n    halt.",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "pl-complex-001",
  "stdout": "=== Prolog Complex Test ===\n\n1. Family relationships:\n   Tom is Bob's father\n   Tom's grandchildren: [ann,pat]\n   Tom's descendants: [bob,liz,ann,pat,jim]\n\n2. Fibonacci sequence:\n   First 10: 0 1 1 2 3 5 8 13 21 34 \n   Fib(20) = 6765\n\n3. List operations:\n   Length of [1,2,3,4,5,6,7,8,9,10]: 10\n   Sum: 55\n   Reversed: [10,9,8,7,6,5,4,3,2,1]\n\n4. QuickSort:\n   Input:  [64,34,25,12,22,11,90]\n   Output: [11,12,22,25,34,64,90]\n\n5. Pattern matching:\n   Ann's siblings: [pat]\n   All males: [tom,bob,jim]\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  - SWI-Prolog only
  - No external libraries
  - Memory limit: 512 MiB

:::

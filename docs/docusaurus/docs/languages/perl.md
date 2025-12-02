---
title: 'Perl'
description: 'Perl code execution'
---

## Overview

Perl is a highly capable scripting language known for text processing. LLM-Firecracker provides Perl for executing Perl scripts.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `perl:5.40-slim` |
| Version | Perl 5.40.3 |
| Rootfs Size | 300 MB |
| Execution | Interpreted |
| File Extension | `.pl` |
| Run Command | `perl {file}` |
| Execution Time | ~10ms |

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name perl --image perl:5.40-slim --size 300
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang perl --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang perl --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang perl --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang perl --code 'print "Hello from Perl $^V\n";' --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "perl-hello-001",
  "lang": "perl",
  "code": "#!/usr/bin/perl\nuse strict;\nuse warnings;\n\nprint \"Hello from Perl!\\n\";",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "perl-hello-001",
  "stdout": "Hello from Perl!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Arrays and Loops

```json title="Request"
{
  "trace_id": "perl-array-001",
  "lang": "perl",
  "code": "#!/usr/bin/perl\nuse strict;\nuse warnings;\n\nmy @numbers = (1, 2, 3, 4, 5);\nmy $sum = 0;\n$sum += $_ for @numbers;\n\nprint \"Numbers: @numbers\\n\";\nprint \"Sum: $sum\\n\";",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "perl-array-001",
  "stdout": "Numbers: 1 2 3 4 5\nSum: 15\n",
  "stderr": "",
  "exit_code": 0
}
```

### Hashes

```json title="Request"
{
  "trace_id": "perl-hash-001",
  "lang": "perl",
  "code": "#!/usr/bin/perl\nuse strict;\nuse warnings;\n\nmy %user = (\n    name => 'Alice',\n    age  => 30,\n    city => 'NYC'\n);\n\nforeach my $key (keys %user) {\n    print \"$key: $user{$key}\\n\";\n}",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "perl-hash-001",
  "stdout": "name: Alice\nage: 30\ncity: NYC\n",
  "stderr": "",
  "exit_code": 0
}
```

### Regular Expressions

```json title="Request"
{
  "trace_id": "perl-regex-001",
  "lang": "perl",
  "code": "#!/usr/bin/perl\nuse strict;\nuse warnings;\n\nmy $text = 'Hello World, hello universe';\n\n# Count matches\nmy $count = () = $text =~ /hello/gi;\nprint \"Matches: $count\\n\";\n\n# Replace\nmy $replaced = $text;\n$replaced =~ s/hello/hi/gi;\nprint \"Replaced: $replaced\\n\";",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "perl-regex-001",
  "stdout": "Matches: 2\nReplaced: hi World, hi universe\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Comprehensive Perl

```json title="Request"
{
  "trace_id": "perl-complex-001",
  "lang": "perl",
  "code": "#!/usr/bin/perl\nuse strict;\nuse warnings;\nuse List::Util qw(sum reduce);\n\nprint \"=== Perl Complex Test ===\\n\\n\";\n\n# Test 1: Fibonacci with memoization\nprint \"1. Fibonacci with memoization:\\n\";\nmy %memo;\nsub fib {\n    my ($n) = @_;\n    return $memo{$n} if exists $memo{$n};\n    return $n if $n <= 1;\n    $memo{$n} = fib($n - 1) + fib($n - 2);\n    return $memo{$n};\n}\nmy @fibs = map { fib($_) } (0..14);\nprint \"   First 15: @fibs\\n\";\nprint \"   Fib(50) = \" . fib(50) . \"\\n\";\n\n# Test 2: QuickSort\nprint \"\\n2. QuickSort:\\n\";\nsub quicksort {\n    my @arr = @_;\n    return @arr if @arr <= 1;\n    my $pivot = $arr[@arr / 2];\n    my @left = grep { $_ < $pivot } @arr;\n    my @middle = grep { $_ == $pivot } @arr;\n    my @right = grep { $_ > $pivot } @arr;\n    return (quicksort(@left), @middle, quicksort(@right));\n}\nmy @unsorted = (64, 34, 25, 12, 22, 11, 90);\nprint \"   Input:  @unsorted\\n\";\nmy @sorted = quicksort(@unsorted);\nprint \"   Output: @sorted\\n\";\n\n# Test 3: Hash operations\nprint \"\\n3. Hash operations:\\n\";\nmy @people = (\n    { name => 'Alice', age => 30, city => 'NYC' },\n    { name => 'Bob', age => 25, city => 'LA' },\n    { name => 'Charlie', age => 35, city => 'Chicago' }\n);\nforeach my $p (@people) {\n    print \"   $p->{name} ($p->{age}) from $p->{city}\\n\";\n}\nmy %by_city = map { $_->{city} => $_->{name} } @people;\nprint \"   By city: \";\nwhile (my ($k, $v) = each %by_city) {\n    print \"$k=>$v \";\n}\nprint \"\\n\";\n\n# Test 4: Array operations\nprint \"\\n4. Array operations:\\n\";\nmy @numbers = (1..10);\nmy @squares = map { $_ ** 2 } @numbers;\nmy @evens = grep { $_ % 2 == 0 } @numbers;\nmy $sum = sum(@squares);\nprint \"   Numbers: @numbers\\n\";\nprint \"   Squares: @squares\\n\";\nprint \"   Evens: @evens\\n\";\nprint \"   Sum of squares: $sum\\n\";\n\n# Test 5: References and data structures\nprint \"\\n5. References:\\n\";\nmy $matrix = [\n    [1, 2, 3],\n    [4, 5, 6],\n    [7, 8, 9]\n];\nprint \"   Matrix:\\n\";\nforeach my $row (@$matrix) {\n    print \"   [@$row]\\n\";\n}\nmy $sum_matrix = 0;\n$sum_matrix += $_ for map { @$_ } @$matrix;\nprint \"   Sum: $sum_matrix\\n\";\n\n# Test 6: Regular expressions\nprint \"\\n6. Regular expressions:\\n\";\nmy $text = 'Contact: alice@example.com or bob@test.org';\nmy @emails = $text =~ /([\\w.]+\\@[\\w.]+)/g;\nprint \"   Text: $text\\n\";\nprint \"   Emails found: @emails\\n\";\nmy $replaced = $text;\n$replaced =~ s/\\@/[at]/g;\nprint \"   Replaced: $replaced\\n\";\n\n# Test 7: Subroutine references\nprint \"\\n7. Subroutine references:\\n\";\nmy $multiplier = sub {\n    my ($factor) = @_;\n    return sub { my ($x) = @_; return $x * $factor; };\n};\nmy $double = $multiplier->(2);\nmy $triple = $multiplier->(3);\nprint \"   double(5) = \" . $double->(5) . \"\\n\";\nprint \"   triple(5) = \" . $triple->(5) . \"\\n\";\n\n# Test 8: String operations\nprint \"\\n8. String operations:\\n\";\nmy $str = 'Hello, World!';\nprint \"   Original: $str\\n\";\nprint \"   Upper: \" . uc($str) . \"\\n\";\nprint \"   Lower: \" . lc($str) . \"\\n\";\nprint \"   Length: \" . length($str) . \"\\n\";\nprint \"   Reversed: \" . reverse($str) . \"\\n\";\n\nprint \"\\n=== All tests passed ===\\n\";",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "perl-complex-001",
  "stdout": "=== Perl Complex Test ===\n\n1. Fibonacci with memoization:\n   First 15: 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377\n   Fib(50) = 12586269025\n\n2. QuickSort:\n   Input:  64 34 25 12 22 11 90\n   Output: 11 12 22 25 34 64 90\n\n3. Hash operations:\n   Alice (30) from NYC\n   Bob (25) from LA\n   Charlie (35) from Chicago\n   By city: NYC=>Alice LA=>Bob Chicago=>Charlie \n\n4. Array operations:\n   Numbers: 1 2 3 4 5 6 7 8 9 10\n   Squares: 1 4 9 16 25 36 49 64 81 100\n   Evens: 2 4 6 8 10\n   Sum of squares: 385\n\n5. References:\n   Matrix:\n   [1 2 3]\n   [4 5 6]\n   [7 8 9]\n   Sum: 45\n\n6. Regular expressions:\n   Text: Contact: alice@example.com or bob@test.org\n   Emails found: alice@example.com bob@test.org\n   Replaced: Contact: alice[at]example.com or bob[at]test.org\n\n7. Subroutine references:\n   double(5) = 10\n   triple(5) = 15\n\n8. String operations:\n   Original: Hello, World!\n   Upper: HELLO, WORLD!\n   Lower: hello, world!\n   Length: 13\n   Reversed: !dlroW ,olleH\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  The Perl environment has the following limitations:

:::

1. **Core modules only**: No CPAN packages
2. **No network**: LWP/HTTP operations will fail
3. **Memory limit**: 512 MiB
4. **Timeout**: Configurable, default 10 seconds

## Best Practices


    Always include `use strict; use warnings;` for safer code.



    Declare variables with `my` for proper scoping.



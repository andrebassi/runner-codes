---
title: 'Haskell'
description: 'Haskell functional programming with GHC'
---

## Overview

Haskell is a purely functional programming language with strong static typing. LLM-Firecracker provides GHC (Glasgow Haskell Compiler).

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `haskell:9.4-slim` |
| Compiler | GHC 9.4.8 |
| Rootfs Size | 3000 MB |
| Execution | Compiled (ghc + run) |
| File Extension | `.hs` |
| Run Command | `ghc -o runbin {file} && ./runbin` |
| Execution Time | ~813ms |

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name haskell --image haskell:9.4-slim --size 3000
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang haskell --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang haskell --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang haskell --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang haskell --code 'main = putStrLn "Hello from Haskell"' --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "hs-hello-001",
  "lang": "haskell",
  "code": "main :: IO ()\nmain = putStrLn \"Hello from Haskell!\"",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "hs-hello-001",
  "stdout": "Hello from Haskell!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Functional Data Processing

```json title="Request"
{
  "trace_id": "hs-complex-001",
  "lang": "haskell",
  "code": "import Data.List (sortBy, groupBy)\nimport Data.Ord (comparing)\nimport Data.Function (on)\n\n-- Define data types\ndata Person = Person { name :: String, age :: Int, city :: String } deriving (Show, Eq)\n\n-- Sample data\npeople :: [Person]\npeople = [\n    Person \"Alice\" 30 \"NYC\",\n    Person \"Bob\" 25 \"LA\",\n    Person \"Charlie\" 35 \"NYC\",\n    Person \"Diana\" 28 \"LA\",\n    Person \"Eve\" 32 \"Chicago\"\n  ]\n\n-- Group by city\ngroupByCity :: [Person] -> [(String, [Person])]\ngroupByCity ps = map (\\g -> (city (head g), g)) $ groupBy ((==) `on` city) $ sortBy (comparing city) ps\n\n-- Calculate average age\naverageAge :: [Person] -> Double\naverageAge ps = fromIntegral (sum ages) / fromIntegral (length ages)\n  where ages = map age ps\n\n-- Fibonacci with memoization\nfibs :: [Integer]\nfibs = 0 : 1 : zipWith (+) fibs (tail fibs)\n\nfibonacci :: Int -> Integer\nfibonacci n = fibs !! n\n\n-- QuickSort implementation\nquicksort :: Ord a => [a] -> [a]\nquicksort [] = []\nquicksort (x:xs) = quicksort [y | y <- xs, y < x] ++ [x] ++ quicksort [y | y <- xs, y >= x]\n\nmain :: IO ()\nmain = do\n    putStrLn \"=== Haskell Complex Test ===\"\n    \n    -- Test 1: Data processing\n    putStrLn \"\\n1. People by city:\"\n    mapM_ (\\(c, ps) -> putStrLn $ c ++ \": \" ++ show (map name ps)) (groupByCity people)\n    \n    -- Test 2: Average age\n    putStrLn $ \"\\n2. Average age: \" ++ show (averageAge people)\n    \n    -- Test 3: Fibonacci\n    putStrLn \"\\n3. First 15 Fibonacci numbers:\"\n    print $ take 15 fibs\n    putStrLn $ \"Fibonacci(50) = \" ++ show (fibonacci 50)\n    \n    -- Test 4: QuickSort\n    let unsorted = [64, 34, 25, 12, 22, 11, 90]\n    putStrLn $ \"\\n4. QuickSort \" ++ show unsorted\n    putStrLn $ \"   Result: \" ++ show (quicksort unsorted)\n    \n    -- Test 5: List comprehensions and higher-order functions\n    let nums = [1..10]\n    putStrLn \"\\n5. Functional operations on [1..10]:\"\n    putStrLn $ \"   Squares: \" ++ show (map (^2) nums)\n    putStrLn $ \"   Even only: \" ++ show (filter even nums)\n    putStrLn $ \"   Sum: \" ++ show (foldr (+) 0 nums)\n    \n    putStrLn \"\\n=== All tests passed ===\"",
  "timeout": 45
}
```

```json title="Response"
{
  "trace_id": "hs-complex-001",
  "stdout": "=== Haskell Complex Test ===\n\n1. People by city:\nChicago: [\"Eve\"]\nLA: [\"Bob\",\"Diana\"]\nNYC: [\"Alice\",\"Charlie\"]\n\n2. Average age: 30.0\n\n3. First 15 Fibonacci numbers:\n[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]\nFibonacci(50) = 12586269025\n\n4. QuickSort [64,34,25,12,22,11,90]\n   Result: [11,12,22,25,34,64,90]\n\n5. Functional operations on [1..10]:\n   Squares: [1,4,9,16,25,36,49,64,81,100]\n   Even only: [2,4,6,8,10]\n   Sum: 55\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Performance

| Operation | Time |
|-----------|------|
| Compilation | ~3s |
| Fibonacci(50) | ~1ms |
| QuickSort (1000 items) | ~10ms |

## Limitations

:::warning

  The Haskell environment has the following limitations:

:::

1. **Base libraries only**: No Hackage packages
2. **GHC compilation overhead**: First run slower
3. **Memory limit**: 512 MiB
4. **Single file**: Must be self-contained

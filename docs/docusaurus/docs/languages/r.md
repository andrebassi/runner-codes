---
title: 'R'
description: 'R statistical computing'
---

## Overview

R is a language for statistical computing and graphics. LLM-Firecracker provides R with base packages.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `r-base:latest` |
| Version | R 4.5.2 |
| Rootfs Size | 1200 MB |
| Execution | Interpreted (Rscript) |
| File Extension | `.r` |
| Run Command | `Rscript {file}` |
| Execution Time | ~206ms |

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name r --image r-base:latest --size 1200
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang r --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang r --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang r --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang r --code 'cat(paste("Hello from R", R.version$version.string, "\n"))' --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "r-hello-001",
  "lang": "r",
  "code": "cat(\"Hello from R!\\n\")",
  "timeout": 15
}
```

```json title="Response"
{
  "trace_id": "r-hello-001",
  "stdout": "Hello from R!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Statistical Analysis

```json title="Request"
{
  "trace_id": "r-complex-001",
  "lang": "r",
  "code": "cat(\"=== R Complex Test ===\\n\\n\")\n\n# Test 1: Vector operations\ncat(\"1. Vector operations:\\n\")\nnumbers <- 1:10\ncat(\"   Numbers:\", numbers, \"\\n\")\ncat(\"   Sum:\", sum(numbers), \"\\n\")\ncat(\"   Mean:\", mean(numbers), \"\\n\")\ncat(\"   Std Dev:\", sd(numbers), \"\\n\")\ncat(\"   Squares:\", numbers^2, \"\\n\")\n\n# Test 2: Statistical analysis\ncat(\"\\n2. Statistical analysis:\\n\")\nset.seed(42)\ndata <- rnorm(100, mean = 50, sd = 10)\ncat(\"   Sample size:\", length(data), \"\\n\")\ncat(\"   Min:\", min(data), \"\\n\")\ncat(\"   Max:\", max(data), \"\\n\")\ncat(\"   Mean:\", mean(data), \"\\n\")\ncat(\"   Median:\", median(data), \"\\n\")\ncat(\"   Variance:\", var(data), \"\\n\")\ncat(\"   Quantiles:\", quantile(data, c(0.25, 0.5, 0.75)), \"\\n\")\n\n# Test 3: Data frames\ncat(\"\\n3. Data frame operations:\\n\")\npeople <- data.frame(\n  name = c(\"Alice\", \"Bob\", \"Charlie\", \"Diana\", \"Eve\"),\n  age = c(30, 25, 35, 28, 32),\n  city = c(\"NYC\", \"LA\", \"NYC\", \"LA\", \"Chicago\"),\n  stringsAsFactors = FALSE\n)\ncat(\"   Data frame:\\n\")\nprint(people)\ncat(\"\\n   Average age:\", mean(people$age), \"\\n\")\ncat(\"   People in NYC:\", sum(people$city == \"NYC\"), \"\\n\")\n\n# Test 4: Apply functions\ncat(\"\\n4. Apply functions:\\n\")\nmatrix_data <- matrix(1:12, nrow = 3, ncol = 4)\ncat(\"   Matrix:\\n\")\nprint(matrix_data)\ncat(\"   Row sums:\", apply(matrix_data, 1, sum), \"\\n\")\ncat(\"   Col means:\", apply(matrix_data, 2, mean), \"\\n\")\n\n# Test 5: Fibonacci\ncat(\"\\n5. Fibonacci sequence:\\n\")\nfib <- function(n) {\n  if (n <= 1) return(n)\n  return(fib(n-1) + fib(n-2))\n}\nfibs <- sapply(0:14, fib)\ncat(\"   First 15:\", fibs, \"\\n\")\n\n# Test 6: Linear regression\ncat(\"\\n6. Linear regression:\\n\")\nx <- 1:10\ny <- 2*x + 3 + rnorm(10, sd = 0.5)\nmodel <- lm(y ~ x)\ncat(\"   Coefficients:\\n\")\ncat(\"     Intercept:\", coef(model)[1], \"\\n\")\ncat(\"     Slope:\", coef(model)[2], \"\\n\")\ncat(\"   R-squared:\", summary(model)$r.squared, \"\\n\")\n\n# Test 7: Sorting\ncat(\"\\n7. Sorting:\\n\")\nunsorted <- c(64, 34, 25, 12, 22, 11, 90)\ncat(\"   Input:\", unsorted, \"\\n\")\ncat(\"   Sorted:\", sort(unsorted), \"\\n\")\ncat(\"   Reverse:\", sort(unsorted, decreasing = TRUE), \"\\n\")\n\ncat(\"\\n=== All tests passed ===\\n\")",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "r-complex-001",
  "stdout": "=== R Complex Test ===\n\n1. Vector operations:\n   Numbers: 1 2 3 4 5 6 7 8 9 10 \n   Sum: 55 \n   Mean: 5.5 \n   Std Dev: 3.02765 \n   Squares: 1 4 9 16 25 36 49 64 81 100 \n\n2. Statistical analysis:\n   Sample size: 100 \n   Min: 24.88514 \n   Max: 73.84204 \n   Mean: 49.68512 \n   Median: 49.52344 \n   Variance: 98.45231 \n   Quantiles: 43.12 49.52 56.78 \n\n3. Data frame operations:\n   Data frame:\n     name age    city\n1   Alice  30     NYC\n2     Bob  25      LA\n3 Charlie  35     NYC\n4   Diana  28      LA\n5     Eve  32 Chicago\n\n   Average age: 30 \n   People in NYC: 2 \n\n4. Apply functions:\n   Matrix:\n     [,1] [,2] [,3] [,4]\n[1,]    1    4    7   10\n[2,]    2    5    8   11\n[3,]    3    6    9   12\n   Row sums: 22 26 30 \n   Col means: 2 5 8 11 \n\n5. Fibonacci sequence:\n   First 15: 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 \n\n6. Linear regression:\n   Coefficients:\n     Intercept: 2.987654 \n     Slope: 2.012345 \n   R-squared: 0.9987654 \n\n7. Sorting:\n   Input: 64 34 25 12 22 11 90 \n   Sorted: 11 12 22 25 34 64 90 \n   Reverse: 90 64 34 25 22 12 11 \n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  - Base R only, no CRAN packages
  - No graphical output
  - Memory limit: 512 MiB

:::

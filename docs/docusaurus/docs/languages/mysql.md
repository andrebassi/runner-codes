---
title: 'MySQL'
description: 'MySQL/MariaDB database execution'
---

## Overview

MySQL is one of the most popular relational database management systems. LLM-Firecracker uses MariaDB (a drop-in MySQL replacement) for executing SQL queries in an isolated Firecracker microVM.

## Specifications

| Property | Value |
|----------|-------|
| Base OS | Alpine Linux 3.20 |
| Database | MariaDB 10.11.14 |
| Rootfs Size | 700 MB |
| Memory | 512 MB |
| File Extension | `.sql` |
| Run Command | `mysql-run` (wrapper script) |
| Execution Time | ~1.7s |

```bash title="1. Create Rootfs base"
infra.operator rootfs from-docker --image alpine:3.20 --name mysql --size 700
```

```bash title="2. Mount rootfs"
sudo mkdir -p /tmp/mnt-mysql
sudo mount /srv/firecracker/images/rootfs-mysql.ext4 /tmp/mnt-mysql
```

```bash title="3. Install MariaDB"
sudo chroot /tmp/mnt-mysql apk add --no-cache mariadb mariadb-client
```

```bash title="4. Configure directories"
sudo chroot /tmp/mnt-mysql mkdir -p /var/lib/mysql /run/mysqld
sudo chroot /tmp/mnt-mysql chown -R mysql:mysql /var/lib/mysql /run/mysqld
```

```bash title="5. Initialize database"
sudo chroot /tmp/mnt-mysql mysql_install_db --user=mysql --datadir=/var/lib/mysql
```

```bash title="6. Create wrapper script"
cat << 'SCRIPT' | sudo tee /tmp/mnt-mysql/usr/local/bin/mysql-run > /dev/null
```

```bash title="mysql-run: start MySQL, execute SQL file, return results"
if [ -z "$1" ]; then
    echo "Usage: mysql-run <file.sql>"
    exit 1
fi
SQL_FILE="$1"
```

```bash title="Start MariaDB in background"
mkdir -p /run/mysqld
chown mysql:mysql /run/mysqld
mysqld_safe --user=mysql --datadir=/var/lib/mysql &>/dev/null &
```

```bash title="Wait for MySQL to be ready (max 5 seconds)"
for i in $(seq 1 50); do
    if mysqladmin ping &>/dev/null; then
        break
    fi
    sleep 0.1
done
```

```bash title="Create default database and execute SQL file"
mysql -t -e "CREATE DATABASE IF NOT EXISTS testdb; USE testdb; SOURCE $SQL_FILE;"
EXIT_CODE=$?
```

```bash title="Shutdown MySQL"
mysqladmin shutdown &>/dev/null
sleep 0.5
exit $EXIT_CODE
SCRIPT
sudo chmod +x /tmp/mnt-mysql/usr/local/bin/mysql-run
```

```bash title="7. Sync and unmount"
sync
sudo umount /tmp/mnt-mysql
```

```bash title="8. Create Snapshot"
infra.operator snapshot create --lang mysql --mem 512 --vcpus 1
```

```bash title="9. Upload to S3"
infra.operator rootfs upload --lang mysql --bucket llm-firecracker
infra.operator snapshot upload --lang mysql --bucket llm-firecracker
```

```bash title="10. Test Execution"
infra.operator host --lang mysql --snapshot --mem 512 --vcpus 1 --code "SELECT 'Hello from MySQL!' AS message;"
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "mysql-hello-001",
  "lang": "mysql",
  "code": "SELECT 'Hello from MySQL!' AS message;",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "mysql-hello-001",
  "stdout": "+-------------------+\n| message           |\n+-------------------+\n| Hello from MySQL! |\n+-------------------+\n",
  "stderr": "",
  "exit_code": 0
}
```

### Create and Query Table

```json title="Request"
{
  "trace_id": "mysql-table-001",
  "lang": "mysql",
  "code": "CREATE TABLE EMPLOYEE (\n  empId INTEGER PRIMARY KEY,\n  name TEXT NOT NULL,\n  dept TEXT NOT NULL\n);\n\nINSERT INTO EMPLOYEE VALUES (1, 'Clark', 'Sales');\nINSERT INTO EMPLOYEE VALUES (2, 'Dave', 'Accounting');\nINSERT INTO EMPLOYEE VALUES (3, 'Ava', 'Sales');\n\nSELECT * FROM EMPLOYEE WHERE dept = 'Sales';",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "mysql-table-001",
  "stdout": "+-------+-------+-------+\n| empId | name  | dept  |\n+-------+-------+-------+\n|     1 | Clark | Sales |\n|     3 | Ava   | Sales |\n+-------+-------+-------+\n",
  "stderr": "",
  "exit_code": 0
}
```

### Aggregations

```json title="Request"
{
  "trace_id": "mysql-agg-001",
  "lang": "mysql",
  "code": "CREATE TABLE sales (id INT AUTO_INCREMENT PRIMARY KEY, product VARCHAR(50), amount DECIMAL(10,2));\n\nINSERT INTO sales (product, amount) VALUES \n('Laptop', 1200.00),\n('Phone', 800.00),\n('Laptop', 1100.00),\n('Tablet', 500.00),\n('Phone', 850.00);\n\nSELECT product, COUNT(*) as qty, SUM(amount) as total, AVG(amount) as avg_price\nFROM sales\nGROUP BY product\nORDER BY total DESC;",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "mysql-agg-001",
  "stdout": "+---------+-----+---------+-----------+\n| product | qty | total   | avg_price |\n+---------+-----+---------+-----------+\n| Laptop  |   2 | 2300.00 | 1150.00   |\n| Phone   |   2 | 1650.00 |  825.00   |\n| Tablet  |   1 |  500.00 |  500.00   |\n+---------+-----+---------+-----------+\n",
  "stderr": "",
  "exit_code": 0
}
```

### Joins

```json title="Request"
{
  "trace_id": "mysql-join-001",
  "lang": "mysql",
  "code": "CREATE TABLE customers (id INT PRIMARY KEY, name VARCHAR(100));\nCREATE TABLE orders (id INT PRIMARY KEY, customer_id INT, product VARCHAR(100), amount DECIMAL(10,2));\n\nINSERT INTO customers VALUES (1, 'Alice'), (2, 'Bob'), (3, 'Charlie');\nINSERT INTO orders VALUES (1, 1, 'Laptop', 1200), (2, 1, 'Mouse', 25), (3, 2, 'Keyboard', 75);\n\nSELECT c.name, o.product, o.amount\nFROM customers c\nJOIN orders o ON c.id = o.customer_id\nORDER BY c.name, o.product;",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "mysql-join-001",
  "stdout": "+-------+----------+---------+\n| name  | product  | amount  |\n+-------+----------+---------+\n| Alice | Laptop   | 1200.00 |\n| Alice | Mouse    |   25.00 |\n| Bob   | Keyboard |   75.00 |\n+-------+----------+---------+\n",
  "stderr": "",
  "exit_code": 0
}
```

## How It Works

The `mysql-run` wrapper script performs the following steps:

1. **Start MariaDB**: Launches `mysqld_safe` in background
2. **Wait for Ready**: Polls `mysqladmin ping` until server responds (max 5 seconds)
3. **Create Database**: Creates `testdb` database if it doesn't exist
4. **Execute SQL**: Runs the user's SQL file with table formatting (`-t` flag)
5. **Shutdown**: Gracefully shuts down MariaDB with `mysqladmin shutdown`

:::note

  The `testdb` database is created automatically at runtime. Each execution starts with a fresh database.

:::

## Limitations

:::warning

  The MySQL environment has the following limitations:

:::

1. **Fresh database each run**: Data is not persisted between executions
2. **Single database**: Only `testdb` database is available
3. **No network**: Remote connections not supported
4. **Memory limit**: 512 MiB for the entire microVM
5. **Startup time**: ~1.5s overhead to start MariaDB

## Best Practices

**Always CREATE TABLE before INSERT statements. Each execution is a fresh database:**

Wrap related INSERT/UPDATE statements in BEGIN/COMMIT for better performance.

**MariaDB is MySQL-compatible, but some edge cases may differ. Use standard SQL when possible:**

## Executor Configuration
```go title="Go code"
// pkg/guest/executor.go
"mysql": {
    Name:      "mysql",
    Extension: ".sql",
    Command:   "/usr/local/bin/mysql-run",
    Args:      []string{},
},
```

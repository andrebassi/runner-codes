#!/usr/bin/env node
/**
 * mysql-run.js - MySQL SQL Syntax Validator using node-sql-parser
 *
 * This script validates and formats MySQL SQL queries WITHOUT requiring
 * a real database connection. It uses the node-sql-parser library to:
 * 1. Parse and validate SQL syntax
 * 2. Format SQL for readability
 * 3. Return AST (Abstract Syntax Tree) on request
 *
 * Usage: mysql-run.js <file.sql>
 */

const fs = require('fs');
const { Parser } = require('node-sql-parser');

// Initialize parser with MySQL dialect
const parser = new Parser();
const DATABASE = 'MySQL';

function main() {
    // Get SQL file from arguments
    const args = process.argv.slice(2);
    if (args.length === 0) {
        console.error('Usage: mysql-run.js <file.sql>');
        process.exit(1);
    }

    const sqlFile = args[0];

    // Read SQL file
    let sql;
    try {
        sql = fs.readFileSync(sqlFile, 'utf8').trim();
    } catch (err) {
        console.error(`Error reading file: ${err.message}`);
        process.exit(1);
    }

    if (!sql) {
        console.error('Error: Empty SQL file');
        process.exit(1);
    }

    // Parse and validate SQL
    try {
        // Parse the SQL (validates syntax)
        const ast = parser.astify(sql, { database: DATABASE });

        // Format the SQL for output
        const formatted = parser.sqlify(ast, { database: DATABASE });

        // Output results
        console.log('=== SQL Validation Result ===');
        console.log('Status: VALID');
        console.log('');
        console.log('=== Formatted SQL ===');
        console.log(formatted);
        console.log('');

        // Show statement info
        if (Array.isArray(ast)) {
            console.log(`=== Statement Summary ===`);
            console.log(`Total statements: ${ast.length}`);
            ast.forEach((stmt, i) => {
                console.log(`  ${i + 1}. ${stmt.type || 'unknown'}`);
            });
        } else {
            console.log('=== Statement Summary ===');
            console.log(`Statement type: ${ast.type || 'unknown'}`);
        }

        process.exit(0);
    } catch (err) {
        // Syntax error
        console.log('=== SQL Validation Result ===');
        console.log('Status: INVALID');
        console.log('');
        console.log('=== Error Details ===');
        console.error(`Syntax Error: ${err.message}`);

        // Try to extract line/column info if available
        if (err.location) {
            console.error(`Location: Line ${err.location.start.line}, Column ${err.location.start.column}`);
        }

        process.exit(1);
    }
}

main();

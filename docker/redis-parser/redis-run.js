#!/usr/bin/env node
/**
 * redis-run.js - Redis Commands Syntax Validator
 *
 * This script validates Redis commands WITHOUT requiring a real Redis connection.
 * It parses Redis commands and validates their syntax.
 *
 * Usage: redis-run.js <file.redis>
 *
 * File format: One command per line (RESP protocol not required)
 * Example:
 *   SET key value
 *   GET key
 *   HSET myhash field value
 */

const fs = require('fs');

// Redis commands with argument patterns
// Format: { minArgs, maxArgs (-1 for unlimited), description }
const REDIS_COMMANDS = {
    // String commands
    'SET': { min: 2, max: -1, desc: 'SET key value [EX|PX|EXAT|PXAT time] [NX|XX] [KEEPTTL] [GET]' },
    'GET': { min: 1, max: 1, desc: 'GET key' },
    'MSET': { min: 2, max: -1, desc: 'MSET key value [key value ...]' },
    'MGET': { min: 1, max: -1, desc: 'MGET key [key ...]' },
    'INCR': { min: 1, max: 1, desc: 'INCR key' },
    'INCRBY': { min: 2, max: 2, desc: 'INCRBY key increment' },
    'INCRBYFLOAT': { min: 2, max: 2, desc: 'INCRBYFLOAT key increment' },
    'DECR': { min: 1, max: 1, desc: 'DECR key' },
    'DECRBY': { min: 2, max: 2, desc: 'DECRBY key decrement' },
    'APPEND': { min: 2, max: 2, desc: 'APPEND key value' },
    'STRLEN': { min: 1, max: 1, desc: 'STRLEN key' },
    'GETRANGE': { min: 3, max: 3, desc: 'GETRANGE key start end' },
    'SETRANGE': { min: 3, max: 3, desc: 'SETRANGE key offset value' },
    'SETNX': { min: 2, max: 2, desc: 'SETNX key value' },
    'SETEX': { min: 3, max: 3, desc: 'SETEX key seconds value' },
    'PSETEX': { min: 3, max: 3, desc: 'PSETEX key milliseconds value' },
    'GETSET': { min: 2, max: 2, desc: 'GETSET key value' },
    'GETDEL': { min: 1, max: 1, desc: 'GETDEL key' },
    'GETEX': { min: 1, max: -1, desc: 'GETEX key [EXAT|PXAT|EX|PX time] [PERSIST]' },

    // Key commands
    'DEL': { min: 1, max: -1, desc: 'DEL key [key ...]' },
    'EXISTS': { min: 1, max: -1, desc: 'EXISTS key [key ...]' },
    'EXPIRE': { min: 2, max: 3, desc: 'EXPIRE key seconds [NX|XX|GT|LT]' },
    'EXPIREAT': { min: 2, max: 3, desc: 'EXPIREAT key unix-time-seconds [NX|XX|GT|LT]' },
    'PEXPIRE': { min: 2, max: 3, desc: 'PEXPIRE key milliseconds [NX|XX|GT|LT]' },
    'PEXPIREAT': { min: 2, max: 3, desc: 'PEXPIREAT key unix-time-milliseconds [NX|XX|GT|LT]' },
    'TTL': { min: 1, max: 1, desc: 'TTL key' },
    'PTTL': { min: 1, max: 1, desc: 'PTTL key' },
    'PERSIST': { min: 1, max: 1, desc: 'PERSIST key' },
    'KEYS': { min: 1, max: 1, desc: 'KEYS pattern' },
    'SCAN': { min: 1, max: -1, desc: 'SCAN cursor [MATCH pattern] [COUNT count] [TYPE type]' },
    'TYPE': { min: 1, max: 1, desc: 'TYPE key' },
    'RENAME': { min: 2, max: 2, desc: 'RENAME key newkey' },
    'RENAMENX': { min: 2, max: 2, desc: 'RENAMENX key newkey' },
    'COPY': { min: 2, max: -1, desc: 'COPY source destination [DB db] [REPLACE]' },
    'DUMP': { min: 1, max: 1, desc: 'DUMP key' },
    'RESTORE': { min: 3, max: -1, desc: 'RESTORE key ttl serialized-value [REPLACE] [ABSTTL]' },
    'TOUCH': { min: 1, max: -1, desc: 'TOUCH key [key ...]' },
    'UNLINK': { min: 1, max: -1, desc: 'UNLINK key [key ...]' },

    // Hash commands
    'HSET': { min: 3, max: -1, desc: 'HSET key field value [field value ...]' },
    'HGET': { min: 2, max: 2, desc: 'HGET key field' },
    'HMSET': { min: 3, max: -1, desc: 'HMSET key field value [field value ...]' },
    'HMGET': { min: 2, max: -1, desc: 'HMGET key field [field ...]' },
    'HGETALL': { min: 1, max: 1, desc: 'HGETALL key' },
    'HDEL': { min: 2, max: -1, desc: 'HDEL key field [field ...]' },
    'HEXISTS': { min: 2, max: 2, desc: 'HEXISTS key field' },
    'HINCRBY': { min: 3, max: 3, desc: 'HINCRBY key field increment' },
    'HINCRBYFLOAT': { min: 3, max: 3, desc: 'HINCRBYFLOAT key field increment' },
    'HKEYS': { min: 1, max: 1, desc: 'HKEYS key' },
    'HVALS': { min: 1, max: 1, desc: 'HVALS key' },
    'HLEN': { min: 1, max: 1, desc: 'HLEN key' },
    'HSETNX': { min: 3, max: 3, desc: 'HSETNX key field value' },
    'HSCAN': { min: 2, max: -1, desc: 'HSCAN key cursor [MATCH pattern] [COUNT count]' },

    // List commands
    'LPUSH': { min: 2, max: -1, desc: 'LPUSH key element [element ...]' },
    'RPUSH': { min: 2, max: -1, desc: 'RPUSH key element [element ...]' },
    'LPOP': { min: 1, max: 2, desc: 'LPOP key [count]' },
    'RPOP': { min: 1, max: 2, desc: 'RPOP key [count]' },
    'LRANGE': { min: 3, max: 3, desc: 'LRANGE key start stop' },
    'LINDEX': { min: 2, max: 2, desc: 'LINDEX key index' },
    'LLEN': { min: 1, max: 1, desc: 'LLEN key' },
    'LSET': { min: 3, max: 3, desc: 'LSET key index element' },
    'LINSERT': { min: 4, max: 4, desc: 'LINSERT key BEFORE|AFTER pivot element' },
    'LREM': { min: 3, max: 3, desc: 'LREM key count element' },
    'LTRIM': { min: 3, max: 3, desc: 'LTRIM key start stop' },
    'BLPOP': { min: 2, max: -1, desc: 'BLPOP key [key ...] timeout' },
    'BRPOP': { min: 2, max: -1, desc: 'BRPOP key [key ...] timeout' },
    'LMOVE': { min: 4, max: 4, desc: 'LMOVE source destination LEFT|RIGHT LEFT|RIGHT' },
    'BLMOVE': { min: 5, max: 5, desc: 'BLMOVE source destination LEFT|RIGHT LEFT|RIGHT timeout' },

    // Set commands
    'SADD': { min: 2, max: -1, desc: 'SADD key member [member ...]' },
    'SREM': { min: 2, max: -1, desc: 'SREM key member [member ...]' },
    'SMEMBERS': { min: 1, max: 1, desc: 'SMEMBERS key' },
    'SISMEMBER': { min: 2, max: 2, desc: 'SISMEMBER key member' },
    'SMISMEMBER': { min: 2, max: -1, desc: 'SMISMEMBER key member [member ...]' },
    'SCARD': { min: 1, max: 1, desc: 'SCARD key' },
    'SPOP': { min: 1, max: 2, desc: 'SPOP key [count]' },
    'SRANDMEMBER': { min: 1, max: 2, desc: 'SRANDMEMBER key [count]' },
    'SUNION': { min: 1, max: -1, desc: 'SUNION key [key ...]' },
    'SINTER': { min: 1, max: -1, desc: 'SINTER key [key ...]' },
    'SDIFF': { min: 1, max: -1, desc: 'SDIFF key [key ...]' },
    'SUNIONSTORE': { min: 2, max: -1, desc: 'SUNIONSTORE destination key [key ...]' },
    'SINTERSTORE': { min: 2, max: -1, desc: 'SINTERSTORE destination key [key ...]' },
    'SDIFFSTORE': { min: 2, max: -1, desc: 'SDIFFSTORE destination key [key ...]' },
    'SMOVE': { min: 3, max: 3, desc: 'SMOVE source destination member' },
    'SSCAN': { min: 2, max: -1, desc: 'SSCAN key cursor [MATCH pattern] [COUNT count]' },

    // Sorted Set commands
    'ZADD': { min: 3, max: -1, desc: 'ZADD key [NX|XX] [GT|LT] [CH] [INCR] score member [score member ...]' },
    'ZREM': { min: 2, max: -1, desc: 'ZREM key member [member ...]' },
    'ZSCORE': { min: 2, max: 2, desc: 'ZSCORE key member' },
    'ZRANK': { min: 2, max: 2, desc: 'ZRANK key member' },
    'ZREVRANK': { min: 2, max: 2, desc: 'ZREVRANK key member' },
    'ZRANGE': { min: 3, max: -1, desc: 'ZRANGE key min max [BYSCORE|BYLEX] [REV] [LIMIT offset count] [WITHSCORES]' },
    'ZREVRANGE': { min: 3, max: -1, desc: 'ZREVRANGE key start stop [WITHSCORES]' },
    'ZRANGEBYSCORE': { min: 3, max: -1, desc: 'ZRANGEBYSCORE key min max [WITHSCORES] [LIMIT offset count]' },
    'ZREVRANGEBYSCORE': { min: 3, max: -1, desc: 'ZREVRANGEBYSCORE key max min [WITHSCORES] [LIMIT offset count]' },
    'ZCARD': { min: 1, max: 1, desc: 'ZCARD key' },
    'ZCOUNT': { min: 3, max: 3, desc: 'ZCOUNT key min max' },
    'ZINCRBY': { min: 3, max: 3, desc: 'ZINCRBY key increment member' },
    'ZINTERSTORE': { min: 3, max: -1, desc: 'ZINTERSTORE destination numkeys key [key ...] [WEIGHTS weight] [AGGREGATE SUM|MIN|MAX]' },
    'ZUNIONSTORE': { min: 3, max: -1, desc: 'ZUNIONSTORE destination numkeys key [key ...] [WEIGHTS weight] [AGGREGATE SUM|MIN|MAX]' },
    'ZSCAN': { min: 2, max: -1, desc: 'ZSCAN key cursor [MATCH pattern] [COUNT count]' },
    'ZPOPMIN': { min: 1, max: 2, desc: 'ZPOPMIN key [count]' },
    'ZPOPMAX': { min: 1, max: 2, desc: 'ZPOPMAX key [count]' },
    'BZPOPMIN': { min: 2, max: -1, desc: 'BZPOPMIN key [key ...] timeout' },
    'BZPOPMAX': { min: 2, max: -1, desc: 'BZPOPMAX key [key ...] timeout' },

    // Pub/Sub
    'PUBLISH': { min: 2, max: 2, desc: 'PUBLISH channel message' },
    'SUBSCRIBE': { min: 1, max: -1, desc: 'SUBSCRIBE channel [channel ...]' },
    'UNSUBSCRIBE': { min: 0, max: -1, desc: 'UNSUBSCRIBE [channel [channel ...]]' },
    'PSUBSCRIBE': { min: 1, max: -1, desc: 'PSUBSCRIBE pattern [pattern ...]' },
    'PUNSUBSCRIBE': { min: 0, max: -1, desc: 'PUNSUBSCRIBE [pattern [pattern ...]]' },
    'PUBSUB': { min: 1, max: -1, desc: 'PUBSUB subcommand [argument [argument ...]]' },

    // Transaction
    'MULTI': { min: 0, max: 0, desc: 'MULTI' },
    'EXEC': { min: 0, max: 0, desc: 'EXEC' },
    'DISCARD': { min: 0, max: 0, desc: 'DISCARD' },
    'WATCH': { min: 1, max: -1, desc: 'WATCH key [key ...]' },
    'UNWATCH': { min: 0, max: 0, desc: 'UNWATCH' },

    // Server
    'PING': { min: 0, max: 1, desc: 'PING [message]' },
    'ECHO': { min: 1, max: 1, desc: 'ECHO message' },
    'INFO': { min: 0, max: 1, desc: 'INFO [section]' },
    'DBSIZE': { min: 0, max: 0, desc: 'DBSIZE' },
    'FLUSHDB': { min: 0, max: 1, desc: 'FLUSHDB [ASYNC|SYNC]' },
    'FLUSHALL': { min: 0, max: 1, desc: 'FLUSHALL [ASYNC|SYNC]' },
    'SELECT': { min: 1, max: 1, desc: 'SELECT index' },
    'SAVE': { min: 0, max: 0, desc: 'SAVE' },
    'BGSAVE': { min: 0, max: 1, desc: 'BGSAVE [SCHEDULE]' },
    'LASTSAVE': { min: 0, max: 0, desc: 'LASTSAVE' },
    'TIME': { min: 0, max: 0, desc: 'TIME' },
    'CONFIG': { min: 1, max: -1, desc: 'CONFIG GET|SET|RESETSTAT|REWRITE ...' },
    'CLIENT': { min: 1, max: -1, desc: 'CLIENT ID|KILL|LIST|GETNAME|SETNAME|PAUSE ...' },
    'SHUTDOWN': { min: 0, max: 1, desc: 'SHUTDOWN [NOSAVE|SAVE]' },
    'DEBUG': { min: 1, max: -1, desc: 'DEBUG subcommand [argument]' },
};

function parseCommand(line) {
    // Handle quoted strings
    const tokens = [];
    let current = '';
    let inQuote = false;
    let quoteChar = null;

    for (let i = 0; i < line.length; i++) {
        const char = line[i];

        if (!inQuote && (char === '"' || char === "'")) {
            inQuote = true;
            quoteChar = char;
        } else if (inQuote && char === quoteChar) {
            inQuote = false;
            quoteChar = null;
        } else if (!inQuote && char === ' ') {
            if (current) {
                tokens.push(current);
                current = '';
            }
        } else {
            current += char;
        }
    }

    if (current) {
        tokens.push(current);
    }

    return tokens;
}

function validateCommand(line, lineNumber) {
    const result = {
        line: lineNumber,
        command: null,
        args: [],
        valid: true,
        error: null,
        description: null
    };

    // Parse the line
    const tokens = parseCommand(line);
    if (tokens.length === 0) {
        return null; // Empty line
    }

    const cmd = tokens[0].toUpperCase();
    const args = tokens.slice(1);

    result.command = cmd;
    result.args = args;

    // Check if command exists
    const cmdSpec = REDIS_COMMANDS[cmd];
    if (!cmdSpec) {
        result.valid = false;
        result.error = `Unknown command: ${cmd}`;
        return result;
    }

    result.description = cmdSpec.desc;

    // Check argument count
    if (args.length < cmdSpec.min) {
        result.valid = false;
        result.error = `Too few arguments. Expected at least ${cmdSpec.min}, got ${args.length}`;
        return result;
    }

    if (cmdSpec.max !== -1 && args.length > cmdSpec.max) {
        result.valid = false;
        result.error = `Too many arguments. Expected at most ${cmdSpec.max}, got ${args.length}`;
        return result;
    }

    return result;
}

function main() {
    // Get file from arguments
    const args = process.argv.slice(2);
    if (args.length === 0) {
        console.error('Usage: redis-run.js <file.redis>');
        process.exit(1);
    }

    const commandFile = args[0];

    // Read command file
    let content;
    try {
        content = fs.readFileSync(commandFile, 'utf8');
    } catch (err) {
        console.error(`Error reading file: ${err.message}`);
        process.exit(1);
    }

    // Parse and validate each line
    const lines = content.split('\n');
    const results = [];
    let hasErrors = false;

    lines.forEach((line, i) => {
        const trimmed = line.trim();

        // Skip empty lines and comments
        if (!trimmed || trimmed.startsWith('#')) {
            return;
        }

        const result = validateCommand(trimmed, i + 1);
        if (result) {
            results.push(result);
            if (!result.valid) {
                hasErrors = true;
            }
        }
    });

    // Output results
    console.log('=== Redis Commands Validation ===');
    console.log(`Status: ${hasErrors ? 'INVALID' : 'VALID'}`);
    console.log(`Total commands: ${results.length}`);
    console.log('');

    results.forEach((r) => {
        const status = r.valid ? 'OK' : 'ERROR';
        console.log(`Line ${r.line}: [${status}] ${r.command} ${r.args.join(' ')}`);

        if (r.error) {
            console.log(`  Error: ${r.error}`);
        }

        if (r.description && r.valid) {
            console.log(`  Syntax: ${r.description}`);
        }
    });

    // Summary
    console.log('');
    console.log('=== Summary ===');
    const valid = results.filter(r => r.valid).length;
    const invalid = results.filter(r => !r.valid).length;
    console.log(`Valid: ${valid}/${results.length}`);
    console.log(`Invalid: ${invalid}/${results.length}`);

    process.exit(hasErrors ? 1 : 0);
}

main();

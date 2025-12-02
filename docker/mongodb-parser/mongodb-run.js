#!/usr/bin/env node
/**
 * mongodb-run.js - MongoDB Query Syntax Validator
 *
 * This script validates MongoDB queries WITHOUT requiring a real database connection.
 * It parses JSON queries and validates their structure for MongoDB operations.
 *
 * Supported operations:
 * - find: { collection: "users", filter: {}, projection: {} }
 * - insertOne/insertMany: { collection: "users", document(s): {} }
 * - updateOne/updateMany: { collection: "users", filter: {}, update: {} }
 * - deleteOne/deleteMany: { collection: "users", filter: {} }
 * - aggregate: { collection: "users", pipeline: [] }
 *
 * Usage: mongodb-run.js <file.json>
 */

const fs = require('fs');

// MongoDB operators validation
const QUERY_OPERATORS = [
    '$eq', '$ne', '$gt', '$gte', '$lt', '$lte', '$in', '$nin',
    '$and', '$or', '$not', '$nor', '$exists', '$type',
    '$regex', '$text', '$where', '$all', '$elemMatch', '$size',
    '$expr', '$jsonSchema', '$mod', '$geoIntersects', '$geoWithin',
    '$near', '$nearSphere', '$box', '$center', '$centerSphere', '$geometry',
    '$maxDistance', '$minDistance', '$polygon'
];

const UPDATE_OPERATORS = [
    '$set', '$unset', '$inc', '$mul', '$rename', '$min', '$max',
    '$currentDate', '$setOnInsert', '$addToSet', '$pop', '$pull',
    '$push', '$pullAll', '$each', '$position', '$slice', '$sort',
    '$bit'
];

const AGGREGATION_STAGES = [
    '$match', '$group', '$sort', '$project', '$limit', '$skip',
    '$unwind', '$lookup', '$addFields', '$replaceRoot', '$facet',
    '$bucket', '$bucketAuto', '$count', '$graphLookup', '$sample',
    '$sortByCount', '$merge', '$out', '$redact', '$unionWith'
];

function validateQuery(query) {
    const issues = [];

    // Check for unknown operators in query
    function checkOperators(obj, validOps, context) {
        if (typeof obj !== 'object' || obj === null) return;

        for (const key of Object.keys(obj)) {
            if (key.startsWith('$')) {
                if (!validOps.includes(key)) {
                    issues.push(`Unknown operator '${key}' in ${context}`);
                }
            }
            if (typeof obj[key] === 'object' && obj[key] !== null) {
                checkOperators(obj[key], validOps, context);
            }
        }
    }

    return issues;
}

function validateOperation(op) {
    const result = {
        valid: true,
        operation: null,
        collection: null,
        details: [],
        warnings: []
    };

    // Check for collection
    if (!op.collection || typeof op.collection !== 'string') {
        result.valid = false;
        result.details.push('Missing or invalid "collection" field');
        return result;
    }
    result.collection = op.collection;

    // Detect operation type
    if (op.filter !== undefined || op.projection !== undefined) {
        // find operation
        if (op.document || op.documents || op.update || op.pipeline) {
            result.warnings.push('Mixed operation fields detected');
        }
        result.operation = 'find';
        result.details.push(`Filter: ${JSON.stringify(op.filter || {})}`);
        if (op.projection) {
            result.details.push(`Projection: ${JSON.stringify(op.projection)}`);
        }
        if (op.sort) {
            result.details.push(`Sort: ${JSON.stringify(op.sort)}`);
        }
        if (op.limit) {
            result.details.push(`Limit: ${op.limit}`);
        }
    } else if (op.document !== undefined) {
        result.operation = 'insertOne';
        if (typeof op.document !== 'object' || op.document === null) {
            result.valid = false;
            result.details.push('Invalid "document" field - must be an object');
        } else {
            result.details.push(`Document keys: ${Object.keys(op.document).join(', ')}`);
        }
    } else if (op.documents !== undefined) {
        result.operation = 'insertMany';
        if (!Array.isArray(op.documents)) {
            result.valid = false;
            result.details.push('Invalid "documents" field - must be an array');
        } else {
            result.details.push(`Documents count: ${op.documents.length}`);
        }
    } else if (op.update !== undefined) {
        result.operation = op.multi ? 'updateMany' : 'updateOne';
        if (!op.filter) {
            result.warnings.push('No filter specified - will match first document');
        }
        result.details.push(`Filter: ${JSON.stringify(op.filter || {})}`);
        result.details.push(`Update: ${JSON.stringify(op.update)}`);
        if (op.upsert) {
            result.details.push('Upsert: enabled');
        }
    } else if (op.pipeline !== undefined) {
        result.operation = 'aggregate';
        if (!Array.isArray(op.pipeline)) {
            result.valid = false;
            result.details.push('Invalid "pipeline" field - must be an array');
        } else {
            result.details.push(`Pipeline stages: ${op.pipeline.length}`);
            op.pipeline.forEach((stage, i) => {
                const stageType = Object.keys(stage)[0];
                result.details.push(`  ${i + 1}. ${stageType}`);
            });
        }
    } else if (op.delete !== undefined || op.deleteOne !== undefined || op.deleteMany !== undefined) {
        result.operation = op.multi || op.deleteMany ? 'deleteMany' : 'deleteOne';
        const filter = op.delete || op.deleteOne || op.deleteMany;
        result.details.push(`Filter: ${JSON.stringify(filter)}`);
    } else {
        result.valid = false;
        result.details.push('Unknown operation type. Expected: filter, document, documents, update, or pipeline');
    }

    return result;
}

function main() {
    // Get file from arguments
    const args = process.argv.slice(2);
    if (args.length === 0) {
        console.error('Usage: mongodb-run.js <file.json>');
        process.exit(1);
    }

    const queryFile = args[0];

    // Read query file
    let content;
    try {
        content = fs.readFileSync(queryFile, 'utf8').trim();
    } catch (err) {
        console.error(`Error reading file: ${err.message}`);
        process.exit(1);
    }

    if (!content) {
        console.error('Error: Empty query file');
        process.exit(1);
    }

    // Parse JSON
    let query;
    try {
        query = JSON.parse(content);
    } catch (err) {
        console.log('=== MongoDB Query Validation ===');
        console.log('Status: INVALID');
        console.log('');
        console.log('=== Error Details ===');
        console.error(`JSON Parse Error: ${err.message}`);
        process.exit(1);
    }

    // Validate query
    const results = [];
    const queries = Array.isArray(query) ? query : [query];

    queries.forEach((q, i) => {
        const result = validateOperation(q);
        results.push({ index: i, ...result });
    });

    // Output results
    console.log('=== MongoDB Query Validation ===');
    const allValid = results.every(r => r.valid);
    console.log(`Status: ${allValid ? 'VALID' : 'INVALID'}`);
    console.log('');

    results.forEach((r, i) => {
        console.log(`=== Query ${i + 1} ===`);
        console.log(`Operation: ${r.operation || 'unknown'}`);
        console.log(`Collection: ${r.collection || 'not specified'}`);
        console.log(`Valid: ${r.valid ? 'YES' : 'NO'}`);

        if (r.details.length > 0) {
            console.log('Details:');
            r.details.forEach(d => console.log(`  ${d}`));
        }

        if (r.warnings.length > 0) {
            console.log('Warnings:');
            r.warnings.forEach(w => console.log(`  - ${w}`));
        }
        console.log('');
    });

    process.exit(allValid ? 0 : 1);
}

main();

# MongoDB Query Syntax Validator
# Validates MongoDB queries without a real database connection
FROM node:22-alpine3.20

# Create minimal workspace
WORKDIR /opt/mongodb-parser

# Copy the runner script
COPY mongodb-run.js /usr/local/bin/mongodb-run
RUN chmod +x /usr/local/bin/mongodb-run

# Default command (for testing)
CMD ["node", "--version"]

# Redis Commands Syntax Validator
# Validates Redis commands without a real Redis connection
FROM node:22-alpine3.20

# Create minimal workspace
WORKDIR /opt/redis-parser

# Copy the runner script
COPY redis-run.js /usr/local/bin/redis-run
RUN chmod +x /usr/local/bin/redis-run

# Default command (for testing)
CMD ["node", "--version"]

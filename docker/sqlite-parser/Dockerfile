# SQLite SQL Syntax Validator
# Uses node-sql-parser to validate SQLite queries without a real database
FROM node:22-alpine3.20

# Install node-sql-parser
WORKDIR /opt/sql-parser
RUN npm init -y && npm install node-sql-parser --save

# Copy the runner script
COPY sqlite-run.js /usr/local/bin/sqlite-run
RUN chmod +x /usr/local/bin/sqlite-run

# Set NODE_PATH for node-sql-parser
ENV NODE_PATH=/opt/sql-parser/node_modules

# Default command (for testing)
CMD ["node", "--version"]

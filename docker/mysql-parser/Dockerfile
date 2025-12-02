# MySQL SQL Syntax Validator
# Uses node-sql-parser to validate MySQL queries without a real database
FROM node:22-alpine3.20

# Install node-sql-parser
WORKDIR /opt/sql-parser
RUN npm init -y && npm install node-sql-parser --save

# Copy the runner script
COPY mysql-run.js /usr/local/bin/mysql-run
RUN chmod +x /usr/local/bin/mysql-run

# Create wrapper script for .sql files
RUN echo '#!/bin/sh' > /usr/local/bin/mysql-run.sh && \
    echo 'exec /usr/local/bin/node /usr/local/bin/mysql-run "$@"' >> /usr/local/bin/mysql-run.sh && \
    chmod +x /usr/local/bin/mysql-run.sh

# Set NODE_PATH for node-sql-parser
ENV NODE_PATH=/opt/sql-parser/node_modules

# Default command (for testing)
CMD ["node", "--version"]

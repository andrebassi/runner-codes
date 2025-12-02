# Cloudflare Containers requires linux/amd64
FROM --platform=linux/amd64 node:20-slim

WORKDIR /app

# Install mintlify CLI
RUN npm install -g mintlify

# Copy documentation files
COPY . .

# Expose port 3333
EXPOSE 3333

# Start Mintlify dev server
CMD ["mintlify", "dev", "--port", "3333", "--host", "0.0.0.0"]

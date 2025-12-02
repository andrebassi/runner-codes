import { Container, getContainer } from "@cloudflare/containers";

interface Env {
  DOCS_CONTAINER: DurableObjectNamespace;
}

// Mintlify documentation container
export class MintlifyDocs extends Container {
  defaultPort = 3333;
  sleepAfter = "5m"; // Sleep after 5 minutes of inactivity (scale to zero)
}

export default {
  async fetch(request: Request, env: Env): Promise<Response> {
    // Single instance for docs
    const container = getContainer(env.DOCS_CONTAINER, "docs-main");

    // Forward all requests to the container
    return container.fetch(request);
  },
};

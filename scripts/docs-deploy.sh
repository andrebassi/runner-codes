#!/bin/bash
# Publica o docs.runner.codes (Docusaurus, 4 idiomas) no Cloudflare Pages pelo login OAuth do wrangler desta máquina.
# Uso: scripts/docs-deploy.sh list   -> projetos do Pages da conta (para achar o do docs)
#      scripts/docs-deploy.sh deploy -> build com link quebrado reprovando + upload para o projeto DOCS_PAGES_PROJECT
set -euo pipefail
export PATH="$HOME/.nix-profile/bin:$PATH"
# Conta "Bassi | Global": a única que o login OAuth enxerga (wrangler whoami, 20/10/2026).
export CLOUDFLARE_ACCOUNT_ID="${CLOUDFLARE_ACCOUNT_ID:-e11c18594fd320087af6f18de03012b7}"
# 3.112.0: a 3.114.0 avisa que quebrou o deploy com nodejs_compat; versão fixa evita que o npx pegue outra.
WRANGLER="npx --yes wrangler@3.112.0"
cd "$(dirname "$0")/../docs/docusaurus"

case "${1:-}" in
  list)
    timeout 120s $WRANGLER pages project list
    ;;
  deploy)
    # O projeto do Pages que atende docs.runner.codes tem nome da era antiga (listado em 20/10/2026); trocar o nome
    # exigiria recriar o projeto e o domínio, então fica este.
    DOCS_PAGES_PROJECT="${DOCS_PAGES_PROJECT:-llm-firecracker-docs}"
    timeout 900s npm run build
    # O _redirects precisa ir junto: ele mantém os caminhos antigos apontando para as páginas novas.
    test -f build/_redirects
    timeout 600s $WRANGLER pages deploy build --project-name "$DOCS_PAGES_PROJECT" --branch main --commit-dirty=true
    ;;
  *)
    echo "uso: $0 list|deploy" >&2
    exit 2
    ;;
esac

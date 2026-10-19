import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

// Navegação da documentação: uma categoria por assunto, na ordem de leitura.
const sidebars: SidebarsConfig = {
  docsSidebar: [
    {
      type: 'category',
      label: 'Primeiros passos',
      collapsed: false,
      items: ['introduction', 'quickstart', 'agents', 'architecture'],
    },
    {
      type: 'category',
      label: 'Conceitos',
      items: [
        'concepts/libkrun',
        'concepts/images',
        'concepts/pool',
        'concepts/guest-runner',
        'concepts/vsock-communication',
      ],
    },
    {
      type: 'category',
      label: 'CLI',
      items: ['cli/overview', 'cli/run', 'cli/image', 'cli/http-api'],
    },
    {
      type: 'category',
      label: 'Referência da API',
      items: [
        'api-reference/overview',
        'api-reference/job-request',
        'api-reference/job-response',
        'api-reference/vsock-protocol',
      ],
    },
    {
      type: 'category',
      label: 'Linguagens',
      items: [
        'languages/overview',
        {
          type: 'category',
          label: 'Interpretadas',
          items: [
            'languages/python',
            'languages/nodejs',
            'languages/ruby',
            'languages/php',
            'languages/perl',
          ],
        },
        {
          type: 'category',
          label: 'Compiladas',
          items: ['languages/go', 'languages/rust', 'languages/c', 'languages/cpp', 'languages/java'],
        },
        {
          type: 'category',
          label: 'Shell e utilitários',
          items: ['languages/bash', 'languages/sh', 'languages/awk', 'languages/jq', 'languages/sqlite'],
        },
      ],
    },
    {
      type: 'category',
      label: 'Tasks',
      items: ['tasks/overview', 'tasks/build', 'tasks/testing'],
    },
    {
      type: 'category',
      label: 'Avançado',
      items: ['advanced/performance', 'advanced/gpu', 'advanced/security', 'advanced/test-report', 'advanced/troubleshooting'],
    },
    {
      type: 'category',
      label: 'Guias',
      items: ['guides/database-parsers-setup'],
    },
  ],
};

export default sidebars;

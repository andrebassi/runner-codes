import type {SidebarsConfig} from '@docusaurus/plugin-content-docs';

const sidebars: SidebarsConfig = {
  docsSidebar: [
    {
      type: 'category',
      label: 'Getting Started',
      collapsed: false,
      items: ['introduction', 'quickstart', 'architecture'],
    },
    {
      type: 'category',
      label: 'CLI Reference',
      items: [
        'cli/overview',
        'cli/rootfs',
        'cli/snapshot',
        'cli/run',
        'cli/http-api',
      ],
    },
    {
      type: 'category',
      label: 'Core Concepts',
      items: [
        'concepts/firecracker',
        'concepts/vsock-communication',
        'concepts/rootfs-images',
        'concepts/guest-runner',
      ],
    },
    {
      type: 'category',
      label: 'AWS Deployment',
      items: [
        'aws/overview',
        'aws/ec2-setup',
        'aws/s3-storage',
        'aws/full-pipeline',
      ],
    },
    {
      type: 'category',
      label: 'Tasks Reference',
      items: [
        'tasks/overview',
        'tasks/build',
        'tasks/aws-infrastructure',
        'tasks/rootfs-management',
        'tasks/s3-operations',
        'tasks/testing',
      ],
    },
    {
      type: 'category',
      label: 'Language Support',
      items: [
        'languages/overview',
        'languages/mega-runtime',
        {
          type: 'category',
          label: 'Main Languages',
          items: [
            'languages/python',
            'languages/nodejs',
            'languages/typescript',
            'languages/go',
            'languages/java',
            'languages/bash',
          ],
        },
        {
          type: 'category',
          label: 'Systems Languages',
          items: [
            'languages/rust',
            'languages/c',
            'languages/cpp',
          ],
        },
        {
          type: 'category',
          label: 'JVM Languages',
          items: [
            'languages/kotlin',
            'languages/scala',
          ],
        },
        {
          type: 'category',
          label: 'Scripting Languages',
          items: [
            'languages/ruby',
            'languages/php',
            'languages/perl',
            'languages/lua',
            'languages/tcl',
          ],
        },
        {
          type: 'category',
          label: 'Functional Languages',
          items: [
            'languages/haskell',
            'languages/elixir',
            'languages/erlang',
            'languages/lisp',
            'languages/scheme',
          ],
        },
        {
          type: 'category',
          label: 'Scientific Computing',
          items: [
            'languages/r',
            'languages/julia',
            'languages/octave',
            'languages/fortran',
          ],
        },
        {
          type: 'category',
          label: 'Other Languages',
          items: [
            'languages/crystal',
            'languages/nim',
            'languages/prolog',
            'languages/pascal',
            'languages/sqlite',
            'languages/cobol',
          ],
        },
        {
          type: 'category',
          label: 'Database Parsers',
          items: [
            'languages/mysql-parser',
            'languages/postgresql-parser',
            'languages/mariadb-parser',
            'languages/sqlite-parser',
            'languages/mongodb-parser',
            'languages/redis-parser',
          ],
        },
      ],
    },
    {
      type: 'category',
      label: 'API Reference',
      items: [
        'api-reference/overview',
        'api-reference/job-request',
        'api-reference/job-response',
        'api-reference/vsock-protocol',
      ],
    },
    {
      type: 'category',
      label: 'Advanced',
      items: [
        'advanced/snapshots',
        'advanced/security',
        'advanced/performance',
        'advanced/troubleshooting',
      ],
    },
  ],
};

export default sidebars;

import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'Runner Codes',
  tagline: 'Secure code execution for AI-generated code and any test',
  favicon: 'img/favicon.svg',

  future: {
    v4: true,
  },

  url: 'https://docs.runner.codes',
  baseUrl: '/',

  organizationName: 'andrebassi',
  projectName: 'runner-codes',

  onBrokenLinks: 'warn',
  onBrokenMarkdownLinks: 'warn',

  i18n: {
    defaultLocale: 'en',
    locales: ['en'],
  },

  markdown: {
    format: 'md',
  },

  presets: [
    [
      'classic',
      {
        docs: {
          routeBasePath: '/',
          sidebarPath: './sidebars.ts',
          editUrl: 'https://github.com/andrebassi/runner-codes/tree/main/docs/docusaurus/',
        },
        blog: false,
        theme: {
          customCss: './src/css/custom.css',
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    image: 'img/social-card.png',
    colorMode: {
      defaultMode: 'dark',
      disableSwitch: false,
      respectPrefersColorScheme: false,
    },
    navbar: {
      logo: {
        alt: 'Runner Codes',
        src: 'img/light.svg',
        srcDark: 'img/dark.svg',
      },
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'docsSidebar',
          position: 'left',
          label: 'Documentation',
        },
        {
          to: '/api-reference/overview',
          label: 'API Reference',
          position: 'left',
        },
{
          href: 'https://github.com/andrebassi/runner-codes',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    footer: {
      style: 'dark',
      links: [
        {
          title: 'Documentation',
          items: [
            {label: 'Introduction', to: '/introduction'},
            {label: 'Quickstart', to: '/quickstart'},
            {label: 'Architecture', to: '/architecture'},
          ],
        },
        {
          title: 'Reference',
          items: [
            {label: 'CLI Overview', to: '/cli/overview'},
            {label: 'API Reference', to: '/api-reference/overview'},
            {label: 'Languages', to: '/languages/overview'},
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Runner Codes. Developed and maintained by <a href="https://andrebassi.com.br" target="_blank" rel="noopener noreferrer">André Bassi</a>`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['bash', 'json', 'go', 'rust', 'python', 'typescript', 'toml'],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;

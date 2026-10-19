import {themes as prismThemes} from 'prism-react-renderer';
import type {Config} from '@docusaurus/types';
import type * as Preset from '@docusaurus/preset-classic';

const config: Config = {
  title: 'Runner Codes',
  tagline: 'Execução de código não confiável em microVMs libkrun, uma VM nova por job',
  favicon: 'img/favicon.svg',

  future: {
    v4: true,
  },

  url: 'https://docs.runner.codes',
  baseUrl: '/',

  organizationName: 'andrebassi',
  projectName: 'runner-codes',

  // Link quebrado reprova o build: com 'warn' a documentação publicava links mortos sem aviso.
  onBrokenLinks: 'throw',

  i18n: {
    defaultLocale: 'pt-BR',
    locales: ['pt-BR'],
  },

  markdown: {
    format: 'md',
    hooks: {
      onBrokenMarkdownLinks: 'throw',
    },
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
          label: 'Documentação',
        },
        {
          to: '/api-reference/overview',
          label: 'API',
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
      copyright: `Copyright © ${new Date().getFullYear()} Runner Codes.<br/>Desenvolvido e mantido por <a href="https://andrebassi.com.br" target="_blank" rel="noopener noreferrer">André Bassi</a>`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
      additionalLanguages: ['bash', 'json', 'go', 'rust', 'python', 'typescript', 'toml'],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;

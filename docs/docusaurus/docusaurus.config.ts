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

  // Quatro idiomas: o pt-BR é a fonte; en, es e zh-Hans saem em /en/, /es/ e /zh-Hans/. Página sem tradução
  // em i18n/<locale>/docusaurus-plugin-content-docs/current/ cai no texto em pt-BR, então o build não depende dela.
  i18n: {
    defaultLocale: 'pt-BR',
    locales: ['pt-BR', 'en', 'es', 'zh-Hans'],
    localeConfigs: {
      'pt-BR': {label: '🇧🇷 Português', htmlLang: 'pt-BR'},
      en: {label: '🇺🇸 English', htmlLang: 'en-US'},
      es: {label: '🇪🇸 Español', htmlLang: 'es-ES'},
      'zh-Hans': {label: '🇨🇳 中文', htmlLang: 'zh-CN'},
    },
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
    // Escuro por padrão, como a landing; quem tem o sistema no claro recebe o claro.
    colorMode: {
      defaultMode: 'dark',
      disableSwitch: false,
      respectPrefersColorScheme: true,
    },
    // Barra igual à da landing: logo "runner.codes" em mono (SVG com o ponto azul e o cursor piscando), sem
    // título em texto ao lado, e os links à direita. O título vazio evita repetir o nome ao lado do logo.
    navbar: {
      title: '',
      logo: {
        alt: 'runner.codes',
        src: 'img/light.svg',
        srcDark: 'img/dark.svg',
        width: 138,
        height: 24,
      },
      items: [
        {
          type: 'docSidebar',
          sidebarId: 'docsSidebar',
          position: 'left',
          label: 'Docs',
        },
        {
          to: '/api-reference/overview',
          label: 'API',
          position: 'left',
        },
        {
          href: 'https://github.com/andrebassi/runner-codes/tree/main/examples',
          label: 'Exemplos',
          position: 'left',
        },
        {
          type: 'localeDropdown',
          position: 'right',
        },
        {
          href: 'https://github.com/andrebassi/runner-codes',
          label: 'GitHub',
          position: 'right',
        },
      ],
    },
    // Rodapé enxuto: uma linha de links e o crédito.
    footer: {
      style: 'dark',
      links: [
        {label: 'runner.codes', href: 'https://runner.codes'},
        {label: 'Docs', to: '/introduction'},
        {label: 'API', to: '/api-reference/overview'},
        {label: 'GitHub', href: 'https://github.com/andrebassi/runner-codes'},
      ],
      copyright: `© ${new Date().getFullYear()} runner.codes · <a href="https://andrebassi.com.br" target="_blank" rel="noopener noreferrer">André Bassi</a>`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.vsDark,
      additionalLanguages: ['bash', 'json', 'go', 'rust', 'python', 'typescript', 'toml'],
    },
  } satisfies Preset.ThemeConfig,
};

export default config;

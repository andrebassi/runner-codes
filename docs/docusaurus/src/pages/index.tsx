import type {ReactNode} from 'react';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import Translate, {translate} from '@docusaurus/Translate';
import styles from './index.module.css';

/** Cartão da grade de seções: título, uma linha do que a seção cobre e o destino (doc interno ou URL externa). */
type SectionCard = {
  title: string;
  description: string;
  to?: string;
  href?: string;
};

/**
 * Seções que a home oferece, na ordem de leitura das docs. Os títulos que são nome de comando ou de arquivo
 * (CLI, runner.code, Pack, API, SDKs) não passam por tradução; o resto sai do JSON de cada idioma.
 * Os caminhos internos são conferidos pelo build (onBrokenLinks: 'throw'): renomear um doc quebra aqui.
 */
function sectionCards(): SectionCard[] {
  return [
    {
      title: 'CLI',
      description: translate({
        id: 'home.cards.cli.description',
        message: 'rc run, machine, image e serve: o job efêmero e a máquina persistente pelo terminal.',
        description: 'Descrição do cartão da CLI na home',
      }),
      to: '/cli/overview',
    },
    {
      title: 'runner.code',
      description: translate({
        id: 'home.cards.runnerCode.description',
        message: 'A máquina declarada num arquivo TOML: imagem, recursos, env, init e workload.',
        description: 'Descrição do cartão do arquivo runner.code na home',
      }),
      to: '/cli/runner-code',
    },
    {
      title: translate({
        id: 'home.cards.network.title',
        message: 'Rede e volumes',
        description: 'Título do cartão de rede e volumes na home',
      }),
      description: translate({
        id: 'home.cards.network.description',
        message: 'Nasce sem rede e sem nada do host. Saída por host permitido, portas e diretórios montados.',
        description: 'Descrição do cartão de rede e volumes na home',
      }),
      to: '/cli/network',
    },
    {
      title: translate({
        id: 'home.cards.credentials.title',
        message: 'Credenciais',
        description: 'Título do cartão de credenciais na home',
      }),
      description: translate({
        id: 'home.cards.credentials.description',
        message: 'Tokens que o código usa sem conseguir ler: o valor real nunca entra na VM.',
        description: 'Descrição do cartão de credenciais na home',
      }),
      to: '/cli/credentials',
    },
    {
      title: 'Pack',
      description: translate({
        id: 'home.cards.pack.description',
        message: 'Uma máquina inteira num executável portátil, que roda em outro host sem pull nem build.',
        description: 'Descrição do cartão do pack na home',
      }),
      to: '/cli/pack',
    },
    {
      title: 'API',
      description: translate({
        id: 'home.cards.api.description',
        message: 'rc serve: endpoints HTTP, formato do job e da resposta, erros e limites.',
        description: 'Descrição do cartão da API na home',
      }),
      to: '/api-reference/overview',
    },
    {
      title: 'SDKs',
      description: translate({
        id: 'home.cards.sdks.description',
        message: 'Clientes Go e Python, só com biblioteca padrão, para o job e para as máquinas.',
        description: 'Descrição do cartão dos SDKs na home',
      }),
      to: '/api-reference/sdks',
    },
    {
      title: translate({
        id: 'home.cards.examples.title',
        message: 'Exemplos e guias',
        description: 'Título do cartão de exemplos e guias na home',
      }),
      description: translate({
        id: 'home.cards.examples.description',
        message: 'Projetos prontos para rodar (apps, navegador headless, LLM local, SDKs) e guias passo a passo.',
        description: 'Descrição do cartão de exemplos e guias na home',
      }),
      href: 'https://github.com/andrebassi/runner-codes/tree/main/examples',
    },
    {
      title: translate({
        id: 'home.cards.agents.title',
        message: 'Agentes',
        description: 'Título do cartão da referência para agentes na home',
      }),
      description: translate({
        id: 'home.cards.agents.description',
        message: 'Referência curta e completa para quem opera o rc, gente ou agente de IA.',
        description: 'Descrição do cartão da referência para agentes na home',
      }),
      to: '/agents',
    },
  ];
}

/** Os três comandos do início rápido: instala a libkrun e compila, constrói a imagem e roda o primeiro job. */
const QUICKSTART_LINES = [
  'task krun:install && task build',
  'task image:build -- --stack python',
  "./bin/rc run --stack python --code 'print(2**10)'",
];

/** Topo da home: selo, título em duas linhas (a segunda em gradiente), subtítulo e os dois botões. */
function Hero(): ReactNode {
  return (
    <header className={styles.hero}>
      <span className={styles.kicker}>
        <i aria-hidden="true" />
        <Translate id="home.hero.kicker" description="Selo acima do título da home">
          MicroVMs · libkrun · código aberto
        </Translate>
      </span>
      <h1 className={styles.title}>
        <Translate id="home.hero.title.line1" description="Primeira linha do título da home">
          Rode qualquer código.
        </Translate>
        <br />
        <span className={styles.grad}>
          <Translate id="home.hero.title.line2" description="Segunda linha do título da home, em destaque">
            Nada sai da caixa.
          </Translate>
        </span>
      </h1>
      <p className={styles.lead}>
        <Translate id="home.hero.lead" description="Subtítulo da home">
          Cada execução ganha uma microVM própria: kernel separado, sem rede, disco só leitura, de pé em ~260 ms e
          destruída no fim. Quando precisar de mais, uma máquina persistente com rede filtrada por host.
        </Translate>
      </p>
      <div className={styles.ctas}>
        <Link className={`${styles.btn} ${styles.primary}`} to="/quickstart">
          <Translate id="home.hero.cta.start" description="Botão principal da home, leva ao início rápido">
            Começar
          </Translate>
        </Link>
        <Link className={styles.btn} href="https://runner.codes">
          runner.codes ↗
        </Link>
      </div>
    </header>
  );
}

/** Grade de cartões com uma entrada por seção da documentação. */
function Sections(): ReactNode {
  return (
    <section className={styles.section}>
      <div className={styles.eyebrow}>
        <Translate id="home.sections.eyebrow" description="Rótulo acima da grade de seções">
          Documentação
        </Translate>
      </div>
      <h2 className={styles.h2}>
        <Translate id="home.sections.title" description="Título da grade de seções">
          Por onde começar
        </Translate>
      </h2>
      <div className={styles.grid}>
        {sectionCards().map((card) => (
          <Link key={card.title} className={styles.card} to={card.to} href={card.href}>
            <h3>
              {card.title}
              {card.href ? <span className={styles.ext}>↗</span> : null}
            </h3>
            <p>{card.description}</p>
          </Link>
        ))}
      </div>
    </section>
  );
}

/** Terminal com os três comandos para começar, no visual do terminal da landing. */
function QuickstartTerminal(): ReactNode {
  return (
    <section className={styles.section}>
      <div className={styles.eyebrow}>
        <Translate id="home.quickstart.eyebrow" description="Rótulo acima do bloco de comandos">
          Em 3 comandos
        </Translate>
      </div>
      <h2 className={styles.h2}>
        <Translate id="home.quickstart.title" description="Título do bloco de comandos">
          Compile, construa a imagem e rode
        </Translate>
      </h2>
      <div className={styles.term}>
        <div className={styles.tbar} aria-hidden="true">
          <i style={{background: '#ff5f57'}} />
          <i style={{background: '#febc2e'}} />
          <i style={{background: '#28c840'}} />
          <span>runner-codes</span>
        </div>
        <pre className={styles.tbody}>
          {QUICKSTART_LINES.map((line) => (
            <div key={line}>
              <span className={styles.prompt}>$ </span>
              {line}
            </div>
          ))}
          <div className={styles.out}>1024</div>
        </pre>
      </div>
      <p className={styles.more}>
        <Link to="/quickstart">
          <Translate id="home.quickstart.more" description="Link para o início rápido completo">
            Pré-requisitos e o passo a passo completo →
          </Translate>
        </Link>
      </p>
    </section>
  );
}

/** Home da documentação: hero, grade de seções e o bloco de comandos. */
export default function Home(): ReactNode {
  return (
    <Layout
      title={translate({id: 'home.meta.title', message: 'Documentação', description: 'Título da aba na home'})}
      description={translate({
        id: 'home.meta.description',
        message: 'Execução de código não confiável em microVMs, uma VM nova por job, sem rede e com root só leitura.',
        description: 'Descrição meta da home',
      })}>
      <main className={styles.wrap}>
        <Hero />
        <Sections />
        <QuickstartTerminal />
      </main>
    </Layout>
  );
}

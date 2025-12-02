# Plano de Implementação: Todas as Linguagens

## Resumo

Implementar suporte completo para **50+ linguagens** mostradas na imagem, incluindo:
- Rootfs images individuais
- Configuração no guest-runner (executor.go)
- Tasks no Taskfile.yaml
- Documentação MDX individual
- Testes automatizados

## Linguagens Identificadas (da imagem)

### Tier 1: Alta Prioridade (Linguagens Populares)
| # | Linguagem | Tipo | Comando | Extensão | Rootfs Size |
|---|-----------|------|---------|----------|-------------|
| 1 | Java | Compilada | `java` | `.java` | 800 MB |
| 2 | C | Compilada | `gcc` | `.c` | 600 MB |
| 3 | C++ | Compilada | `g++` | `.cpp` | 700 MB |
| 4 | JavaScript | Interpretada | `node` | `.js` | 700 MB |
| 5 | TypeScript | Transpilada | `ts-node` | `.ts` | 800 MB |
| 6 | PHP | Interpretada | `php` | `.php` | 600 MB |

### Tier 2: Média Prioridade (Linguagens Comuns)
| # | Linguagem | Tipo | Comando | Extensão | Rootfs Size |
|---|-----------|------|---------|----------|-------------|
| 7 | Ruby | Interpretada | `ruby` | `.rb` | 600 MB |
| 8 | Perl | Interpretada | `perl` | `.pl` | 500 MB |
| 9 | Scala | Compilada/JVM | `scala` | `.scala` | 900 MB |
| 10 | Kotlin | Compilada/JVM | `kotlin` | `.kt` | 900 MB |
| 11 | Swift | Compilada | `swift` | `.swift` | 1.5 GB |
| 12 | Objective-C | Compilada | `clang` | `.m` | 800 MB |

### Tier 3: Funcional/Científico
| # | Linguagem | Tipo | Comando | Extensão | Rootfs Size |
|---|-----------|------|---------|----------|-------------|
| 13 | Haskell | Compilada | `ghc` | `.hs` | 1.2 GB |
| 14 | Erlang | Interpretada | `erl` | `.erl` | 800 MB |
| 15 | Elixir | Interpretada | `elixir` | `.ex` | 900 MB |
| 16 | Clojure | JVM | `clojure` | `.clj` | 800 MB |
| 17 | OCaml | Compilada | `ocaml` | `.ml` | 700 MB |
| 18 | F# | Compilada/.NET | `dotnet fsi` | `.fs` | 1.5 GB |

### Tier 4: Scripting/Shell
| # | Linguagem | Tipo | Comando | Extensão | Rootfs Size |
|---|-----------|------|---------|----------|-------------|
| 19 | Lua | Interpretada | `lua` | `.lua` | 400 MB |
| 20 | R | Interpretada | `Rscript` | `.r` | 1.2 GB |
| 21 | Tcl | Interpretada | `tclsh` | `.tcl` | 400 MB |
| 22 | Groovy | JVM | `groovy` | `.groovy` | 800 MB |

### Tier 5: Modernas/Emergentes
| # | Linguagem | Tipo | Comando | Extensão | Rootfs Size |
|---|-----------|------|---------|----------|-------------|
| 23 | Dart | Compilada/JIT | `dart` | `.dart` | 800 MB |
| 24 | Deno | Runtime | `deno` | `.ts` | 700 MB |
| 25 | Bun | Runtime | `bun` | `.js` | 600 MB |

### Tier 6: Shells Especiais
| # | Linguagem | Tipo | Comando | Extensão | Rootfs Size |
|---|-----------|------|---------|----------|-------------|
| 26 | sh | Shell | `sh` | `.sh` | 400 MB |
| 27 | JShell | Java REPL | `jshell` | `.jsh` | 800 MB |

### Tier 7: Linguagens Clássicas/Educacionais
| # | Linguagem | Tipo | Comando | Extensão | Rootfs Size |
|---|-----------|------|---------|----------|-------------|
| 28 | Pascal | Compilada | `fpc` | `.pas` | 600 MB |
| 29 | Fortran | Compilada | `gfortran` | `.f90` | 600 MB |
| 30 | Cobol | Compilada | `cobc` | `.cob` | 700 MB |
| 31 | Ada | Compilada | `gnat` | `.adb` | 700 MB |
| 32 | Basic | Interpretada | `freebasic` | `.bas` | 500 MB |

### Tier 8: Assembly/Low-Level
| # | Linguagem | Tipo | Comando | Extensão | Rootfs Size |
|---|-----------|------|---------|----------|-------------|
| 33 | Assembly | Compilada | `nasm` | `.asm` | 500 MB |

### Tier 9: Lógica/Especializadas
| # | Linguagem | Tipo | Comando | Extensão | Rootfs Size |
|---|-----------|------|---------|----------|-------------|
| 34 | Prolog | Interpretada | `swipl` | `.pl` | 600 MB |
| 35 | Racket | Interpretada | `racket` | `.rkt` | 800 MB |
| 36 | CommonLisp | Interpretada | `sbcl` | `.lisp` | 700 MB |

### Tier 10: Altamente Especializadas
| # | Linguagem | Tipo | Comando | Extensão | Rootfs Size |
|---|-----------|------|---------|----------|-------------|
| 37 | D | Compilada | `dmd` | `.d` | 800 MB |
| 38 | CoffeeScript | Transpilada | `coffee` | `.coffee` | 700 MB |
| 39 | EJS | Template | `ejs` | `.ejs` | 700 MB |
| 40 | Octave | Interpretada | `octave` | `.m` | 1.0 GB |

### Tier 11: Microsoft/.NET
| # | Linguagem | Tipo | Comando | Extensão | Rootfs Size |
|---|-----------|------|---------|----------|-------------|
| 41 | C# | Compilada/.NET | `dotnet` | `.cs` | 1.5 GB |
| 42 | VB.NET | Compilada/.NET | `dotnet` | `.vb` | 1.5 GB |

### Tier 12: Exóticas
| # | Linguagem | Tipo | Comando | Extensão | Rootfs Size |
|---|-----------|------|---------|----------|-------------|
| 43 | BrainFK | Interpretada | `bf` | `.bf` | 400 MB |
| 44 | Text | N/A | `cat` | `.txt` | 400 MB |

---

## Arquitetura de Implementação

### 1. Modificações no executor.go

```go
// Novas linguagens a adicionar no NewExecutor()
languages: map[string]LanguageConfig{
    // Tier 1
    "java": {
        Name: "java", Extension: ".java",
        NeedsBuild: true, BuildCmd: "javac", BuildArgs: []string{},
        RunCmd: "java", // Executa a classe
    },
    "c": {
        Name: "c", Extension: ".c",
        NeedsBuild: true, BuildCmd: "gcc", BuildArgs: []string{"-o"},
    },
    "cpp": {
        Name: "cpp", Extension: ".cpp",
        NeedsBuild: true, BuildCmd: "g++", BuildArgs: []string{"-o"},
    },
    "typescript": {
        Name: "typescript", Extension: ".ts",
        Command: "ts-node", Args: []string{},
    },
    "php": {
        Name: "php", Extension: ".php",
        Command: "php", Args: []string{},
    },
    // ... mais linguagens
}
```

### 2. Estrutura de Rootfs por Linguagem

```
/srv/firecracker/images/
├── rootfs-python.ext4      # 600 MB
├── rootfs-nodejs.ext4      # 700 MB
├── rootfs-go.ext4          # 700 MB
├── rootfs-rust.ext4        # 2 GB
├── rootfs-bash.ext4        # 512 MB
├── rootfs-java.ext4        # 800 MB    [NOVO]
├── rootfs-c.ext4           # 600 MB    [NOVO]
├── rootfs-cpp.ext4         # 700 MB    [NOVO]
├── rootfs-typescript.ext4  # 800 MB    [NOVO]
├── rootfs-php.ext4         # 600 MB    [NOVO]
├── rootfs-ruby.ext4        # 600 MB    [NOVO]
├── rootfs-perl.ext4        # 500 MB    [NOVO]
├── rootfs-scala.ext4       # 900 MB    [NOVO]
├── rootfs-kotlin.ext4      # 900 MB    [NOVO]
├── rootfs-swift.ext4       # 1.5 GB    [NOVO]
├── rootfs-haskell.ext4     # 1.2 GB    [NOVO]
├── rootfs-erlang.ext4      # 800 MB    [NOVO]
├── rootfs-elixir.ext4      # 900 MB    [NOVO]
├── rootfs-clojure.ext4     # 800 MB    [NOVO]
├── rootfs-ocaml.ext4       # 700 MB    [NOVO]
├── rootfs-lua.ext4         # 400 MB    [NOVO]
├── rootfs-r.ext4           # 1.2 GB    [NOVO]
├── rootfs-dart.ext4        # 800 MB    [NOVO]
├── rootfs-deno.ext4        # 700 MB    [NOVO]
├── rootfs-bun.ext4         # 600 MB    [NOVO]
├── rootfs-csharp.ext4      # 1.5 GB    [NOVO]
├── rootfs-fsharp.ext4      # 1.5 GB    [NOVO]
└── ... (mais linguagens)
```

### 3. Tasks no Taskfile.yaml

Para cada linguagem:
- `aws:create-rootfs-{lang}`: Cria rootfs e upload S3
- `aws:test-{lang}`: Testa execução

### 4. Documentação MDX

Para cada linguagem, criar arquivo em `docs/mintlify/languages/{lang}.mdx`:
- Overview
- Specifications (Version, Rootfs Size, Execution Type)
- Examples (Hello World, Functions, Error Handling)
- Available Libraries
- Performance metrics
- Limitations
- Best Practices

---

## Estimativas

| Item | Quantidade | Tempo Estimado |
|------|------------|----------------|
| Modificar executor.go | 45+ linguagens | 2-3 horas |
| Criar tasks Taskfile | 45+ tasks | 3-4 horas |
| Criar rootfs images | 45+ images | 8-12 horas (AWS) |
| Documentação MDX | 45+ arquivos | 6-8 horas |
| Testes | 45+ testes | 4-6 horas |
| **TOTAL** | | **24-36 horas** |

---

## Plano de Execução

### Fase 1: Infraestrutura Base
1. Atualizar `executor.go` com todas as linguagens
2. Adicionar host-agent selection logic
3. Criar template de task para rootfs

### Fase 2: Rootfs por Tier (em ordem)
1. **Tier 1**: Java, C, C++, TypeScript, PHP (5 linguagens)
2. **Tier 2**: Ruby, Perl, Scala, Kotlin, Swift (5 linguagens)
3. **Tier 3**: Haskell, Erlang, Elixir, Clojure, OCaml (5 linguagens)
4. **Tier 4-12**: Restantes em ordem de popularidade

### Fase 3: Testes e Validação
1. Testar cada linguagem individualmente
2. Criar suite de testes automatizada
3. Benchmark de performance

### Fase 4: Documentação
1. Criar MDX para cada linguagem
2. Atualizar overview de linguagens
3. Atualizar navegação do Mintlify

---

## Priorização Sugerida

Para começar imediatamente, recomendo implementar nesta ordem:

1. **Java** - Linguagem mais popular, muitos LLMs geram código Java
2. **C/C++** - Fundamentais, usadas para algoritmos
3. **TypeScript** - Demanda crescente
4. **PHP** - Muito código legado e exemplos
5. **Ruby** - Popular para scripts
6. **Scala/Kotlin** - Ecossistema JVM
7. **Swift** - Desenvolvimento iOS
8. **C#/.NET** - Ecossistema Microsoft

---

## Próximos Passos

1. [ ] Aprovar este plano
2. [ ] Iniciar com Tier 1 (Java, C, C++, TypeScript, PHP)
3. [ ] Criar template reutilizável para tasks e docs
4. [ ] Executar criação de rootfs em paralelo na EC2
5. [ ] Documentar progressivamente

**Aguardando aprovação para iniciar implementação.**

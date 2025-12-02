---
title: "JavaScript / Node.js"
description: "Node.js 20.19.6 - Runtime JavaScript"
---

# JavaScript / Node.js

:::info

**Versao**: Node.js 20.19.6
**Rootfs**: 700 MB
**Tipo**: Interpretada

:::

## Overview

JavaScript e a linguagem de programacao mais popular do mundo. Node.js permite executar JavaScript no servidor com performance excepcional.

## Especificacoes

| Propriedade | Valor |
|-------------|-------|
| Versao | 20.19.6 (LTS) |
| Comando | `node` |
| Extensao | `.js` |
| Rootfs Size | 700 MB |
| Tempo de Execucao | ~150ms |

## Modulos Disponiveis

A instalacao padrao inclui todos os modulos built-in do Node.js:
- `fs`, `path`, `os`
- `http`, `https`, `url`
- `crypto`, `buffer`
- `stream`, `events`

:::warning

Pacotes npm externos nao estao disponiveis na instalacao padrao.

:::

## Exemplos

### Hello World
```javascript title="Simple Hello World output"
console.log("Hello World");
```

### Funcoes Assincronas
```javascript title="Async function with Promise"
async function fetchData() {
    return new Promise(resolve => {
        setTimeout(() => resolve("Data loaded!"), 100);
    });
}

fetchData().then(console.log);
```

### Manipulacao de Arrays
```javascript title="Array map and reduce operations"
const numbers = [1, 2, 3, 4, 5];
const doubled = numbers.map(n => n * 2);
const sum = numbers.reduce((a, b) => a + b, 0);

console.log("Doubled:", doubled);
console.log("Sum:", sum);
```

### Classes
```javascript title="Calculator class with basic operations"
class Calculator {
    add(a, b) { return a + b; }
    subtract(a, b) { return a - b; }
    multiply(a, b) { return a * b; }
    divide(a, b) { return a / b; }
}

const calc = new Calculator();
console.log(calc.multiply(6, 7));
```

## TypeScript

TypeScript tambem e suportado via `ts-node`:

```typescript title="TypeScript interface and type safety example"
interface User {
    name: string;
    age: number;
}

const user: User = { name: "Alice", age: 30 };
console.log(`${user.name} is ${user.age} years old`);
```

## Limitacoes

- Sem acesso a rede (fetch/http requests bloqueados)
- Sem acesso a sistema de arquivos externo
- Timeout padrao de 10 segundos
- Memoria limitada a 512MB

## Best Practices

1. **Evite dependencias externas** - Use apenas modulos built-in
2. **Gerencie promessas** - Use async/await com try/catch
3. **Prefira operacoes sincronas** - Para codigo mais previsivel

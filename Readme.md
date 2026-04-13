# Konane ♟️

Implementação do jogo de tabuleiro **Konane** em Scala, jogável contra o computador.

---

## O que é o Konane?

Konane é um jogo de tabuleiro tradicional havaiano para dois jogadores. As peças brancas e pretas são colocadas de forma intercalada no tabuleiro. O objetivo é deixar o adversário sem jogadas possíveis.

### Regras

- A cada turno, o jogador move uma das suas peças **por cima** de uma peça adversária adjacente, ocupando a posição seguinte (que deve estar vazia).
- O movimento pode ser feito em qualquer uma das **4 direções**: cima, baixo, esquerda ou direita.
- A peça adversária **"comida"** é removida do tabuleiro.
- Perde o jogador que, na sua vez, **não tiver jogadas possíveis**.

> Uma jogada válida implica obrigatoriamente comer uma peça do adversário e terminar numa casa vazia.

---

## Como jogar

Nesta implementação, o utilizador joga sempre com as **peças brancas**. O computador joga com as **peças pretas** e realiza **sempre o primeiro turno**.

### Fluxo de jogo

1. **Turno do computador** — o computador realiza automaticamente a sua jogada.
2. **Turno do utilizador:**
   - Aparece o prompt `Peca a mover:` — introduza as coordenadas da peça que deseja mover.
   - Aparece o prompt `Para onde:` — introduza as coordenadas do destino.
3. Se a jogada for **válida**, o turno passa para o computador.
4. Se a jogada for **inválida**, surge a mensagem `!!! JOGADA INVALIDA !!!` e os prompts são repetidos.

### Formato das coordenadas

As coordenadas são introduzidas com a **letra da coluna** seguida do **número da linha**.

```
Exemplos: A1  a1  B3  c5
```

> O jogo **não é case sensitive** — `A1` e `a1` são equivalentes.

---

## Configuração do tabuleiro

O tamanho do tabuleiro pode ser alterado no ficheiro `KonaneTest.scala`, modificando os valores de `r` (linhas) e `c` (colunas).

| Parâmetro | Mínimo | Máximo |
|-----------|--------|--------|
| Linhas (`r`) | 3 | 20 |
| Colunas (`c`) | 3 | 20 |

```scala
val r = 6  // número de linhas
val c = 6  // número de colunas
```

---

O Konane é um jogo de tabuleiro com peças brancas e pretas intrecaladas.
a cada turno o jogador poderá mexer  uma das suas peças, por cima da do adversario (para a posição adjacente, desde
esta se encontre vazia), podendo se mexer em qualquer uma das 4 direções (cima, baixo, esquerda e direita).
Ao realizar esta ação a peça do adversario é removida do tabuleiro (comida) e o turno troca
Um jogador que chegue ao seu turno e não tenha jogadas possiveis perde (jogada possivel entenda-se comer a peça do adversario)

Nesta implementação apenas se pode jogar contra o computador (jogador das peças pretas e 1º turno).
No seu truno irá aparecer o prompt "Peca a mover:" ao que deve escolher a posição da peça que tenciona mover.
para escolher a mesma deverá colocar a letra da coluna e o numero da linha que se encontra (exemplo A1 ou a1, o jogo não é case sensitive)
ao carregar no enter para dar o input ao progama, aparecerá o prompt "Para onde:" e deverá colocar as coordenadas para onde desejar
movimentar a peça escolhida (da mesma maneira que na escolha da peça)(exemplo: A3)
Caso a jogada seja valida(i.e comer uma peça do adversario e ficar num espaço que se encontrava vazio) o turno passará para o computador que fazrá a sua jogada
caso contrario, dirá "!!! JOGADA INVALIDA !!!" e vai pedir novamente os inputs para realizar o seu turno
O tamanho do tabuleiro é variavel, podendo ser alterado no ficheiro "KonaneTest.scala" alterando o val r e val c
O tabuleiro tem um tamanho minimo de 3 colunas/linhas
e um tamanho maximo de 20 colunas/linhas
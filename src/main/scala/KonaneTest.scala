import KonaneLogic.*

import scala.annotation.tailrec

object KonaneTest extends App {
  val r = 8
  val c = 8
  val initialSeed = MyRandom(12345L)

  // 1. Inicialização e Setup
  val fullBoard = initBoard(r, c)
  val (boardReady, openCoords, randAfterSetup) = setupBoard(fullBoard, r, c, initialSeed)

  println("--- JOGO INICIADO ---")
  println(boardToString(boardReady, r, c))

  // 2. Ciclo de Jogo Recursivo
  @tailrec
  def gameLoop(board: Board, rand: MyRandom, currentPlayer: Stone, openCoords: List[Coord2D]): Unit = {
    // Tenta fazer uma jogada aleatória
    val (newBoardOpt, nextRand, nextOpen, move) = playRandomly(
      board, rand, currentPlayer, openCoords, randomMove
    )

    move match {
      case Some(to) =>
        println(s"Jogador ${currentPlayer} jogou para: $to")
        val nextBoard = newBoardOpt.get
        println(boardToString(nextBoard, r, c))

        // Alterna o jogador e continua
        val nextPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
        gameLoop(nextBoard, nextRand, nextPlayer, nextOpen)

      case None =>
        println(s"O jogador $currentPlayer não tem mais jogadas!")
        val winner = if (currentPlayer == Stone.Black) "Brancas (W)" else "Pretas (B)"
        println(s"--- FIM DE JOGO. VENCEDOR: $winner ---")
    }
  }

  // Começar o jogo com as Pretas (regra do Konane)
  gameLoop(boardReady, randAfterSetup, Stone.Black, openCoords)
}
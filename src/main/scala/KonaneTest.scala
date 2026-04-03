import KonaneLogic.*

import scala.annotation.tailrec

object KonaneTest extends App {
  val r = 8
  val c = 8
  if (!isValidDimension(r, c)) {
    println("Erro: Dimensoes invalidas (" + r + " x " + c + "). Devem estar entre 3 e 20.")
  } else {
    val initialSeed = MyRandom(r)
    val fullBoard = initBoard(r, c)
    val (boardReady, openCoords, randAfterSetup) = setupBoard(fullBoard, r, c, initialSeed)

    println("--- JOGO INICIADO ---")
    gameLoop(boardReady, randAfterSetup, Stone.Black, openCoords, r, c)
  }

  @tailrec
  def gameLoop(board: Board, rand: MyRandom, currentPlayer: Stone, openCoords: List[Coord2D], rows: Int, cols: Int): Unit = {
    println(boardToString(board, rows, cols))

    getWinner(board, currentPlayer, rows, cols) match {
      case Some(winner) =>
        val winnerName = if (winner == Stone.Black) "Pretas (B)" else "Brancas (W)"
        println("--- FIM DE JOGO ---")
        println("Vencedor: " + winnerName)

      case None =>
        currentPlayer match {
          case Stone.Black =>
            println("Sua vez (Pretas - B):")
            print("Peca a mover: ")
            val fromStr = scala.io.StdIn.readLine()
            print("Para onde: ")
            val toStr = scala.io.StdIn.readLine()

            processTurn(board, currentPlayer, fromStr, toStr, rows, cols, openCoords) match {
              case Some((newBoard, newOpen)) =>
                gameLoop(newBoard, rand, Stone.White, newOpen, rows, cols)
              case None =>
                println("\n!!! JOGADA INVALIDA !!!\n")
                gameLoop(board, rand, Stone.Black, openCoords, rows, cols)
            }

          case Stone.White =>
            println("Vez do Computador (Brancas - W)...")
            val (newBoardOpt, nextRand, nextOpen, move) = playRandomly(
              board, rand, currentPlayer, openCoords, randomMove
            )

            move match {
              case Some(to) =>
                println("O computador jogou para: " + coordToString(to))
                gameLoop(newBoardOpt.get, nextRand, Stone.Black, nextOpen, rows, cols)
              case None =>
                gameLoop(board, nextRand, Stone.Black, openCoords, rows, cols)
            }
        }
    }
  }
}
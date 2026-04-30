import KonaneLogic.*

import scala.annotation.tailrec

object KonaneTest extends App {
  val r = 6
  val c = 6
  if (!isValidDimension(r, c)) {
    println("Erro: Dimensoes invalidas (" + r + " x " + c + "). Devem estar entre 3 e 20.")
  } else {
    val initialSeed = MyRandom(System.currentTimeMillis())
    val fullBoard = initBoard(r, c)
    val (boardReady, openCoords, randAfterSetup) = setupBoard(fullBoard, r, c, initialSeed)

    println("--- JOGO INICIADO ---")
    gameLoop(boardReady, randAfterSetup, Stone.Black, openCoords, r, c)
  }

  @tailrec
  def gameLoop(board: Board, rand: MyRandom, currentPlayer: Stone, openCoords: List[Coord2D], rows: Int, cols: Int, showBoard: Boolean = true, history: List[(Board, MyRandom, Stone, List[Coord2D])] = Nil): Unit = {
    if (showBoard) println(boardToString(board, rows, cols))

    getWinner(board, currentPlayer, rows, cols) match {
      case Some(winner) =>
        val winnerName = if (winner == Stone.Black) "Pretas (B)" else "Brancas (W)"
        println("--- FIM DE JOGO ---")
        println("Vencedor: " + winnerName)

      case None =>
        currentPlayer match {
          case Stone.Black =>
            println("Sua vez (Pretas - B):")
            print("Peca a mover (ou digite 'undo' para desfazer): ")
            val fromStr = scala.io.StdIn.readLine()

            if (fromStr.trim.toLowerCase == "undo") {
              val toPop = Math.min(2, history.length)
              if (toPop == 0) {
                gameLoop(board, rand, currentPlayer, openCoords, rows, cols, showBoard = true, history)
              } else {
                val restored = history(toPop - 1)
                val newHistory = history.drop(toPop)
                val (b0, r0, p0, open0) = restored
                gameLoop(b0, r0, Stone.Black, open0, rows, cols, showBoard = true, newHistory)
              }
            } else {
              val historyAfterPush = (board, rand, currentPlayer, openCoords) :: history

              print("Para onde: ")
              val toStr = scala.io.StdIn.readLine()

              processTurn(board, currentPlayer, fromStr, toStr, rows, cols, openCoords) match {
                case Some((newBoard, newOpen)) =>
                val parsedTo = parseInput(toStr).get

                println(boardToString(newBoard, rows, cols))

                @tailrec
                def humanChain(currBoard: Board, currOpen: List[Coord2D], lastPos: Coord2D): (Board, List[Coord2D]) = {
                  val possible = getValidMovesForPiece(currBoard, Stone.Black, lastPos, rows, cols)
                  if (possible.isEmpty) (currBoard, currOpen)
                  else {
                    @tailrec
                    def askYesNo(): Boolean = {
                      print("A peca em " + coordToString(lastPos) + " pode capturar novamente. Deseja capturar? (y/n): ")
                      scala.io.StdIn.readLine().trim.toLowerCase match {
                        case "y" => true
                        case "n" => false
                        case _ =>
                          println("Resposta invalida. Responda 'y' ou 'n'.")
                          askYesNo()
                      }
                    }

                    if (askYesNo()) {
                      if (possible.length == 1) {
                        val nextTo = possible.head
                        val mid = ((lastPos._1 + nextTo._1) / 2, (lastPos._2 + nextTo._2) / 2)
                        val (b2, o2) = executeMove(currBoard, Stone.Black, lastPos, nextTo, mid, currOpen)
                        println(boardToString(b2, rows, cols))
                        humanChain(b2, o2, nextTo)
                      } else {
                        print("Para onde: ")
                        val nextToStr = scala.io.StdIn.readLine()
                        parseInput(nextToStr) match {
                          case Some(nextTo) if possible.contains(nextTo) =>
                            val mid = ((lastPos._1 + nextTo._1) / 2, (lastPos._2 + nextTo._2) / 2)
                            val (b2, o2) = executeMove(currBoard, Stone.Black, lastPos, nextTo, mid, currOpen)
                            println(boardToString(b2, rows, cols))
                            humanChain(b2, o2, nextTo)
                          case _ =>
                            println("Jogada invalida para esta peca.")
                            println(boardToString(currBoard, rows, cols))
                            humanChain(currBoard, currOpen, lastPos)
                        }
                      }
                    } else {
                      (currBoard, currOpen)
                    }
                  }
                }

                val (finalBoard, finalOpen) = humanChain(newBoard, newOpen, parsedTo)
                gameLoop(finalBoard, rand, Stone.White, finalOpen, rows, cols, showBoard = false, historyAfterPush)

                case None =>
                  println("\n!!! JOGADA INVALIDA !!!\n")
                  gameLoop(board, rand, Stone.Black, openCoords, rows, cols, showBoard = true, history)
              }
            }

          case Stone.White =>
            println("Vez do Computador (Brancas - W)...")
            val historyAfterPushForComputer = (board, rand, currentPlayer, openCoords) :: history

            val (newBoardOpt, nextRand, nextOpen, move) = playRandomly(
              board, rand, currentPlayer, openCoords, randomMove
            )

            move match {
              case Some(to) =>
                println("O computador jogou para: " + coordToString(to))
                println(boardToString(newBoardOpt.get, rows, cols))
                @tailrec
                def computerChain(currBoardOpt: Option[Board], r: MyRandom, lastPos: Coord2D, currOpen: List[Coord2D]): (Option[Board], MyRandom, List[Coord2D]) = {
                  val boardNow = currBoardOpt.get
                  val possible = getValidMovesForPiece(boardNow, Stone.White, lastPos, rows, cols)
                  if (possible.isEmpty) (currBoardOpt, r, currOpen)
                  else {
                    val (choice, r1) = r.nextInt(2)
                    if (choice == 0) (currBoardOpt, r1, currOpen)
                    else {
                      println("O computador decidiu capturar novamente.")
                      val (chosenTo, r2) = randomMove(possible, r1)
                      val mid = ((lastPos._1 + chosenTo._1) / 2, (lastPos._2 + chosenTo._2) / 2)
                      val (b2, o2) = executeMove(boardNow, Stone.White, lastPos, chosenTo, mid, currOpen)
                      println("O computador jogou para: " + coordToString(chosenTo))
                      println(boardToString(b2, rows, cols))
                      computerChain(Some(b2), r2, chosenTo, o2)
                    }
                  }
                }

                val (finalBoardOpt, finalRand, finalOpen) = computerChain(newBoardOpt, nextRand, to, nextOpen)
                gameLoop(finalBoardOpt.get, finalRand, Stone.Black, finalOpen, rows, cols, showBoard = false, historyAfterPushForComputer)
              case None =>
                gameLoop(board, nextRand, Stone.Black, openCoords, rows, cols, showBoard = true, history)
            }
        }
    }
  }
}
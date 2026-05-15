import scala.annotation.tailrec

object Difficulty {

  trait Strategy {
    def choose(possible: List[Coord2D], r: MyRandom): (Coord2D, MyRandom)
  }

  // Random strategy: used for both Easy and Medium modes
  class RandomStrategy extends Strategy {
    def choose(possible: List[Coord2D], r: MyRandom) =
      if (possible.isEmpty) ((0, 0), r)
      else {
        val (i, r2) = r.nextInt(possible.length)
        (possible(i), r2)
      }
  }

  // Minimax strategy with alpha-beta pruning and depth 6 for Hard difficulty
  class MiniMaxStrategy(board: Board, player: Stone, rows: Int, cols: Int, openCoords: List[Coord2D]) extends Strategy {

    private val MaxDepth = 6

    // Improved evaluation function with horizon effect mitigation
    private def evaluatePosition(b: Board, p: Stone): Int = {
      val opp = if (p == Stone.Black) Stone.White else Stone.Black
      val myMoves = KonaneLogic.getAllValidMoves(b, p, rows, cols).length
      val oppMoves = KonaneLogic.getAllValidMoves(b, opp, rows, cols).length

      // Give decisive weight to terminal positions
      if (oppMoves == 0) Int.MaxValue / 2     // Opponent has no moves - immediate win
      else if (myMoves == 0) Int.MinValue / 2 // We have no moves - immediate loss
      else myMoves - oppMoves                   // Otherwise, evaluate mobility difference
    }

    // Pure minimax with alpha-beta pruning using tail-recursive loop for true early exit
    private def minimax(b: Board, open: List[Coord2D], currentPlayer: Stone, depth: Int,
                       isMaximizing: Boolean, alpha: Int, beta: Int): Int = {
      if (depth == 0) {
        evaluatePosition(b, player)
      } else {
        val moves = KonaneLogic.getAllValidMoves(b, currentPlayer, rows, cols)

        if (moves.isEmpty) {
          evaluatePosition(b, player)
        } else {
          minimaxLoop(moves, if (isMaximizing) Int.MinValue else Int.MaxValue, alpha, beta, isMaximizing, b, open, currentPlayer, depth)
        }
      }
    }

    // Tail-recursive loop for iterating moves with TRUE alpha-beta pruning (early exit)
    @tailrec
    private def minimaxLoop(remainingMoves: List[(Coord2D, Coord2D)], currentScore: Int,
                           alpha: Int, beta: Int, isMaximizing: Boolean, b: Board,
                           open: List[Coord2D], currentPlayer: Stone, depth: Int): Int = {
      remainingMoves match {
        case Nil =>
          // No more moves - return final score
          currentScore

        case _ if alpha >= beta =>
          // ✂️ PRUNING CUT: Alpha-beta bound crossed, stop evaluating
          currentScore

        case (from, to) :: rest =>
          // Evaluate this move
          KonaneLogic.play(b, currentPlayer, from, to, open) match {
            case (Some(nextBoard), nextOpen) =>
              val nextPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
              val score = minimax(nextBoard, nextOpen, nextPlayer, depth - 1, !isMaximizing, alpha, beta)

              // Update bounds based on whose turn it is
              val (newScore, newAlpha, newBeta) = if (isMaximizing) {
                val s = Math.max(currentScore, score)
                (s, Math.max(alpha, s), beta)
              } else {
                val s = Math.min(currentScore, score)
                (s, alpha, Math.min(beta, s))
              }

              // Continue with remaining moves (or prune if bounds crossed)
              minimaxLoop(rest, newScore, newAlpha, newBeta, isMaximizing, b, open, currentPlayer, depth)

            case (None, _) =>
              // Invalid move - skip to next
              minimaxLoop(rest, currentScore, alpha, beta, isMaximizing, b, open, currentPlayer, depth)
          }
      }
    }

    def choose(possible: List[Coord2D], r: MyRandom): (Coord2D, MyRandom) = {
      if (possible.isEmpty) {
        ((0, 0), r)
      } else {
        val pairs = KonaneLogic.getAllValidMoves(board, player, rows, cols)

        // Evaluate each (from, dest) pair directly for precision
        val evaluations = pairs.flatMap { case (from, to) =>
          if (possible.contains(to)) {
            // This destination is reachable from this source
            KonaneLogic.play(board, player, from, to, openCoords) match {
              case (Some(nextBoard), nextOpen) =>
                val nextPlayer = if (player == Stone.Black) Stone.White else Stone.Black
                // Evaluate with alpha-beta pruning
                val score = minimax(nextBoard, nextOpen, nextPlayer, MaxDepth - 1, false, Int.MinValue, Int.MaxValue)
                List((to, score))
              case (None, _) =>
                Nil
            }
          } else {
            Nil
          }
        }

        // Aggregate scores by destination (take maximum for each destination)
        val aggregated = evaluations.foldLeft(Map[Coord2D, Int]()) { case (acc, (dest, score)) =>
          acc.updated(dest, Math.max(acc.getOrElse(dest, Int.MinValue), score))
        }

        // Select best destination(s) - handle empty case safely
        aggregated.values.maxOption match {
          case Some(maxScore) =>
            val best = aggregated.filter(_._2 == maxScore).keys.toList
            val (pickIdx, r2) = r.nextInt(best.length)
            (best(pickIdx), r2)
          case None =>
            // No valid moves evaluated (shouldn't happen, fallback to random from possible)
            val (pickIdx, r2) = r.nextInt(possible.length)
            (possible(pickIdx), r2)
        }
      }
    }
  }

  // Factory: returns a chooser function from the selected Strategy
  def makeChooser(level: String, board: Board, player: Stone, rows: Int, cols: Int, openCoords: List[Coord2D], mode: String = "HVC"): (List[Coord2D], MyRandom) => (Coord2D, MyRandom) = {
    if (mode == "HVH") new RandomStrategy().choose
    else level.toLowerCase match {
      case "facil" | "easy" => new RandomStrategy().choose
      case "medio" | "medium" => new RandomStrategy().choose
      case "dificil" | "hard" => new MiniMaxStrategy(board, player, rows, cols, openCoords).choose
      case _ => new RandomStrategy().choose
    }
  }

}

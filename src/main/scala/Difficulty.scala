import scala.annotation.tailrec

object Difficulty {

    trait Strategy {
        def choose(possible: List[Coord2D], r: MyRandom): (Coord2D, MyRandom)
    }

    class RandomStrategy extends Strategy {
        def choose(possible: List[Coord2D], r: MyRandom) =
            if (possible.isEmpty) ((0, 0), r)
            else {
                val (i, r2) = r.nextInt(possible.length)
                (possible(i), r2)
            }
    }

    class MiniMaxStrategy(board: Board, player: Stone, rows: Int, cols: Int, openCoords: List[Coord2D]) extends Strategy {

        val MaxDepth = 6

        def evaluatePosition(b: Board, p: Stone): Int = {
            val opp = if (p == Stone.Black) Stone.White else Stone.Black
            val myMoves = KonaneLogic.getAllValidMoves(b, p, rows, cols).length
            val oppMoves = KonaneLogic.getAllValidMoves(b, opp, rows, cols).length

            if (oppMoves == 0) Int.MaxValue / 2       // Opponent has no moves - immediate win
            else if (myMoves == 0) Int.MinValue / 2   // We have no moves - immediate loss
            else myMoves - oppMoves                   // Otherwise, evaluate mobility difference
        }

        def minimax(b: Board, open: List[Coord2D], currentPlayer: Stone, depth: Int,
                    isMaximizing: Boolean, alpha: Int, beta: Int): Int = {
            if (depth == 0) {
                evaluatePosition(b, player)
            } else {

                lazy val moves = KonaneLogic.getAllValidMoves(b, currentPlayer, rows, cols)

                if (moves.isEmpty) {
                    evaluatePosition(b, player)
                } else {
                    minimaxLoop(moves, if (isMaximizing) Int.MinValue else Int.MaxValue, alpha, beta, isMaximizing, b, open, currentPlayer, depth)
                }
            }
        }

        @tailrec
        private def minimaxLoop(remainingMoves: List[(Coord2D, Coord2D)], currentScore: Int,
                                alpha: Int, beta: Int, isMaximizing: Boolean, b: Board,
                                open: List[Coord2D], currentPlayer: Stone, depth: Int): Int = {
            remainingMoves match {
                case Nil =>
                    currentScore

                case _ if alpha >= beta =>
                    currentScore

                case (from, to) :: rest =>
                    KonaneLogic.play(b, currentPlayer, from, to, open) match {
                        case (Some(nextBoard), nextOpen) =>
                            val nextPlayer = if (currentPlayer == Stone.Black) Stone.White else Stone.Black
                            val score = minimax(nextBoard, nextOpen, nextPlayer, depth - 1, !isMaximizing, alpha, beta)

                            val (newScore, newAlpha, newBeta) = if (isMaximizing) {
                                val s = Math.max(currentScore, score)
                                (s, Math.max(alpha, s), beta)
                            } else {
                                val s = Math.min(currentScore, score)
                                (s, alpha, Math.min(beta, s))
                            }

                            minimaxLoop(rest, newScore, newAlpha, newBeta, isMaximizing, b, open, currentPlayer, depth)

                        case (None, _) =>
                            minimaxLoop(rest, currentScore, alpha, beta, isMaximizing, b, open, currentPlayer, depth)
                    }
            }
        }

        def choose(possible: List[Coord2D], r: MyRandom): (Coord2D, MyRandom) = {
            if (possible.isEmpty) {
                ((0, 0), r)
            } else {
                val pairs = KonaneLogic.getAllValidMoves(board, player, rows, cols)

                val evaluations = pairs.flatMap { case (from, to) =>
                    if (possible.contains(to)) {
                        KonaneLogic.play(board, player, from, to, openCoords) match {
                            case (Some(nextBoard), nextOpen) =>
                                val nextPlayer = if (player == Stone.Black) Stone.White else Stone.Black
                                val score = minimax(nextBoard, nextOpen, nextPlayer, MaxDepth - 1, false, Int.MinValue, Int.MaxValue)
                                List((to, score))
                            case (None, _) =>
                                Nil
                        }
                    } else {
                        Nil
                    }
                }

                val aggregated = evaluations.foldLeft(Map[Coord2D, Int]()) { case (acc, (dest, score)) =>
                    acc.updated(dest, Math.max(acc.getOrElse(dest, Int.MinValue), score))
                }

                aggregated.values.maxOption match {
                    case Some(maxScore) =>
                        val best = aggregated.filter(_._2 == maxScore).keys.toList
                        val (pickIdx, r2) = r.nextInt(best.length)
                        (best(pickIdx), r2)
                    case None =>
                        val (pickIdx, r2) = r.nextInt(possible.length)
                        (possible(pickIdx), r2)
                }
            }
        }
    }

    def makeChooser(level: String, board: Board, player: Stone, rows: Int, cols: Int, openCoords: List[Coord2D], mode: String = "HVC"): (List[Coord2D], MyRandom) => (Coord2D, MyRandom) = {
        if (mode == "HVH") new RandomStrategy().choose
        else level.toLowerCase match {
            case "facil" => new RandomStrategy().choose
            case "medio" => new RandomStrategy().choose
            case "dificil" => new MiniMaxStrategy(board, player, rows, cols, openCoords).choose
            case _ => new RandomStrategy().choose
        }
    }

}

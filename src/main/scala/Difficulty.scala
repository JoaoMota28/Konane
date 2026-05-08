import scala.annotation.tailrec

object Difficulty {

  trait Strategy {
    def choose(possible: List[Coord2D], r: MyRandom): (Coord2D, MyRandom)
  }

  // Easy: purely random
  class EasyStrategy extends Strategy {
    def choose(possible: List[Coord2D], r: MyRandom) =
      if (possible.isEmpty) ((0, 0), r) else { val (i, r2) = r.nextInt(possible.length); (possible(i), r2) }
  }

  // Medium: purely random (but will do consecutive captures)
  class MediumStrategy extends Strategy {
    def choose(possible: List[Coord2D], r: MyRandom) =
      if (possible.isEmpty) ((0, 0), r) else { val (i, r2) = r.nextInt(possible.length); (possible(i), r2) }
  }

  // Hard: greedy one-step evaluation
  class HardStrategy(board: Board, player: Stone, rows: Int, cols: Int, openCoords: List[Coord2D]) extends Strategy {
    def choose(possible: List[Coord2D], r: MyRandom) =
      if (possible.isEmpty) ((0, 0), r)
      else {
        val pairs = KonaneLogic.getAllValidMoves(board, player, rows, cols)
        val evaluations = possible.map { dest =>
          val sources = pairs.filter(_._2 == dest).map(_._1)
          val scores = sources.flatMap { from =>
            KonaneLogic.applyInitialMovePure(board, player, from, dest, rows, cols, openCoords) match {
              case None => Nil
              case Some((nb, _)) =>
                val opp = if (player == Stone.Black) Stone.White else Stone.Black
                val myMoves = KonaneLogic.getAllValidMoves(nb, player, rows, cols).length
                val oppMoves = KonaneLogic.getAllValidMoves(nb, opp, rows, cols).length
                List(myMoves - oppMoves)
            }
          }
          val bestScore = if (scores.isEmpty) Int.MinValue else scores.max
          (dest, bestScore)
        }

        val maxScore = evaluations.map(_._2).max
        val best = evaluations.filter(_._2 == maxScore).map(_._1)
        val (pickIdx, r2) = r.nextInt(best.length)
        (best(pickIdx), r2)
      }
  }

  // Factory: returns a chooser function from the selected Strategy
  def makeChooser(level: String, board: Board, player: Stone, rows: Int, cols: Int, openCoords: List[Coord2D], mode: String = "HVC"): (List[Coord2D], MyRandom) => (Coord2D, MyRandom) = {
    if (mode == "HVH") new EasyStrategy().choose
    else level.toLowerCase match {
      case "facil" | "easy" => new EasyStrategy().choose
      case "medio" | "medium" => new MediumStrategy().choose
      case "dificil" | "hard" => new HardStrategy(board, player, rows, cols, openCoords).choose
      case _ => new EasyStrategy().choose
    }
  }

}

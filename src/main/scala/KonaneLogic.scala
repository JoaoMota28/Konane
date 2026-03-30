import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.CollectionConverters.ImmutableMapIsParallelizable

type Coord2D = (Int, Int) //(row, column)
type Board = ParMap[Coord2D, Stone]

enum Stone:
  case Black, White

object KonaneLogic {

  def initBoard(rows: Int, cols: Int): Board = {
    def fill(r: Int, c: Int): List[(Coord2D, Stone)] = (r, c) match {
      case (row, _) if row >= rows => Nil
      case (row, col) if col >= cols => fill(row + 1, 0)
      case (row, col) =>
        val stone = (row + col) % 2 match {
          case 0 => Stone.Black
          case _ => Stone.White
        }
        ((row, col), stone) :: fill(row, col + 1)
    }
    fill(0, 0).toMap.par
  }

  //T1
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (randomIndex, nextRand) = rand.nextInt(lstOpenCoords.length)
    val chosenCoord = lstOpenCoords(randomIndex)
    (chosenCoord, nextRand)
  }

  def isValidMove(board: Board, player: Stone, from: Coord2D, to: Coord2D): Boolean = {
    val (rFrom, cFrom) = from
    val (rTo, cTo) = to
    val mid = ((rFrom + rTo) / 2, (cFrom + cTo) / 2)

    val isCorrectDistance = (Math.abs(rFrom - rTo), Math.abs(cFrom - cTo)) match {
      case (2, 0) | (0, 2) => true
      case _ => false
    }

    isCorrectDistance && ((board.get(from), board.get(mid), board.get(to)) match {
      case (Some(player), Some(enemy), None) if enemy != player => true
      case _ => false
    })
  }

  def executeMove(board: Board, player: Stone, from: Coord2D, to: Coord2D, mid: Coord2D, lstOpen: List[Coord2D]): (Board, List[Coord2D]) = {
    val newBoard = board - from - mid + (to -> player)
    val newOpen = (from :: mid :: lstOpen).filterNot(_ == to)
    (newBoard, newOpen)
  }

  //T2
  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    val mid = ((coordFrom._1 + coordTo._1) / 2, (coordFrom._2 + coordTo._2) / 2)

    isValidMove(board, player, coordFrom, coordTo) match {
      case true =>
        val (newBoard, newOpen) = executeMove(board, player, coordFrom, coordTo, mid, lstOpenCoords)
        (Some(newBoard), newOpen)

      case false =>
        (None, lstOpenCoords)
    }

  }
}

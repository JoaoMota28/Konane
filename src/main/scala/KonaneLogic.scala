import scala.annotation.tailrec
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

  def setupBoard(board: Board, rows: Int, cols: Int, rand: MyRandom): (Board, List[Coord2D], MyRandom) = {

    val centers = List((rows / 2, cols / 2), (rows / 2 - 1, cols / 2 - 1), (rows / 2 - 1, cols / 2), (rows / 2, cols / 2 - 1))
    val corners = List((0, 0), (0, cols - 1), (rows - 1, 0), (rows - 1, cols - 1))

    val blackOptions = (centers ++ corners).filter(c => board.get(c).contains(Stone.Black))

    val (idxB, r1) = rand.nextInt(blackOptions.length)
    val blackCoord = blackOptions(idxB)

    val (rb, cb) = blackCoord
    val adjacents = List((rb + 1, cb), (rb - 1, cb), (rb, cb + 1), (rb, cb - 1)).filter(c => board.get(c).contains(Stone.White))

    val (idxW, r2) = r1.nextInt(adjacents.length)
    val whiteCoord = adjacents(idxW)

    (board - blackCoord - whiteCoord, List(blackCoord, whiteCoord), r2)
  }

  def getRows(board: Board): Int = if (board.isEmpty) 0 else board.keys.map(_._1).max + 1

  def getCols(board: Board): Int = if (board.isEmpty) 0 else board.keys.map(_._2).max + 1

  //T1
  def randomMove(lstOpenCoords: List[Coord2D], rand: MyRandom): (Coord2D, MyRandom) = {
    val (randomIndex, nextRand) = rand.nextInt(lstOpenCoords.length)
    val chosenCoord = lstOpenCoords(randomIndex)
    (chosenCoord, nextRand)
  }

  def isValidMove(board: Board, player: Stone, from: Coord2D, to: Coord2D, rows: Int, cols: Int): Boolean = {
    val (rTo, cTo) = to

    if (rTo < 0 || rTo >= rows || cTo < 0 || cTo >= cols) false
    else {
      val (rFrom, cFrom) = from
      val mid = ((rFrom + rTo) / 2, (cFrom + cTo) / 2)

      val isCorrectDistance = (Math.abs(rFrom - rTo), Math.abs(cFrom - cTo)) match {
        case (2, 0) | (0, 2) => true
        case _ => false
      }

      isCorrectDistance && ((board.get(from), board.get(mid), board.get(to)) match {
        case (Some(`player`), Some(enemy), None) if enemy != player => true
        case _ => false
      })
    }
  }

  def executeMove(board: Board, player: Stone,
                  from: Coord2D, to: Coord2D, mid: Coord2D,
                  lstOpen: List[Coord2D]): (Board, List[Coord2D]) = {
    val newBoard = board - from - mid + (to -> player)
    val newOpen = (from :: mid :: lstOpen).filterNot(_ == to)
    (newBoard, newOpen)
  }

  //T2
  def play(board: Board, player: Stone,
           coordFrom: Coord2D, coordTo: Coord2D,
           lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    val mid = ((coordFrom._1 + coordTo._1) / 2, (coordFrom._2 + coordTo._2) / 2)

    val rows = getRows(board)
    val cols = getCols(board)
    
    isValidMove(board, player, coordFrom, coordTo, rows, cols) match {
      case true =>
        val (newBoard, newOpen) = executeMove(board, player, coordFrom, coordTo, mid, lstOpenCoords)
        (Some(newBoard), newOpen)

      case false =>
        (None, lstOpenCoords)
    }
  }

  def getAllValidMoves(board: Board, player: Stone, rows: Int, cols: Int): List[(Coord2D, Coord2D)] = {
    val allCoords = board.keys.toList
    val directions = List((2, 0), (-2, 0), (0, 2), (0, -2))

    def checkDirections(from: Coord2D, ds: List[(Int, Int)]): List[(Coord2D, Coord2D)] = ds match {
      case Nil => Nil
      case (dr, dc) :: tail =>
        val to = (from._1 + dr, from._2 + dc)
        // Agora passamos rows e cols para validar os limites
        if (isValidMove(board, player, from, to, rows, cols))
          (from, to) :: checkDirections(from, tail)
        else
          checkDirections(from, tail)
    }

    def iterateCoords(coords: List[Coord2D]): List[(Coord2D, Coord2D)] = coords match {
      case Nil => Nil
      case head :: tail =>
        checkDirections(head, directions) ++ iterateCoords(tail)
    }

    iterateCoords(allCoords)
  }

  def getDestinations(mvs: List[(Coord2D, Coord2D)]): List[Coord2D] = {

    @tailrec
    def loop(remaining: List[(Coord2D, Coord2D)], acc: List[Coord2D]): List[Coord2D] = remaining match {
      case Nil =>
        acc.reverse

      case (from, to) :: tail =>
        loop(tail, to :: acc)
    }

    loop(mvs, Nil)
  }

  //T3
  def playRandomly(board: Board, r: MyRandom, player: Stone,
                   lstOpenCoords: List[Coord2D],
                   f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)
                  ): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = {

    val rows = getRows(board)
    val cols = getCols(board)
    val moves = getAllValidMoves(board, player, rows, cols)

    moves match {
      case Nil =>
        (None, r, lstOpenCoords, None)

      case _ =>
        val possibleTo = getDestinations(moves)

        val (chosenTo, nextRand) = f(possibleTo, r)

        @tailrec
        def findSource(mvs: List[(Coord2D, Coord2D)], target: Coord2D): Coord2D = mvs match {
          case (from, to) :: _ if to == target => from
          case _ :: tail => findSource(tail, target)
          case Nil => (0, 0)
        }

        val chosenFrom = findSource(moves, chosenTo)

        val (newBoardOpt, newOpen) = play(board, player, chosenFrom, chosenTo, lstOpenCoords)

        (newBoardOpt, nextRand, newOpen, Some(chosenTo))
    }
  }

  //T4
  def boardToString(board: Board, rows: Int, cols: Int): String = {

    val rowRange = (0 until rows).toList
    val colRange = (0 until cols).toList

    rowRange.foldLeft("") { (accBoard, r) =>

      val rowString = colRange.foldLeft("") { (accRow, c) =>
        val symbol = board.get((r, c)) match {
          case Some(Stone.Black) => " B "
          case Some(Stone.White) => " W "
          case None              => " . "
        }

        accRow + symbol
      }

      accBoard + rowString + "\n"
    }
  }
}

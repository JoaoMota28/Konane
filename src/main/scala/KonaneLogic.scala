import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParMap
import scala.collection.parallel.CollectionConverters.ImmutableMapIsParallelizable

type Coord2D = (Int, Int) //(row, column)
type Board = ParMap[Coord2D, Stone]

enum Stone:
  case Black, White

object KonaneLogic {

  // Factory that returns a chooser function for playRandomly according to difficulty.
  // level: "facil" | "medio" | "dificil"
  // The returned function has signature (List[Coord2D], MyRandom) => (Coord2D, MyRandom)
  def makeChooser(level: String, board: Board, player: Stone, rows: Int, cols: Int, openCoords: List[Coord2D], mode: String = "HVC"): (List[Coord2D], MyRandom) => (Coord2D, MyRandom) = {
    Difficulty.makeChooser(level, board, player, rows, cols, openCoords, mode)
  }


  def initBoard(rows: Int, cols: Int): Board = {
    @tailrec
    def fill(r: Int, c: Int, acc: List[(Coord2D, Stone)]): List[(Coord2D, Stone)] = (r, c) match {
      case (row, _) if row >= rows => acc
      case (row, col) if col >= cols => fill(row + 1, 0, acc)
      case (row, col) =>
        val stone = if ((row + col) % 2 == 0) Stone.Black else Stone.White
        fill(row, col + 1, ((row, col), stone) :: acc)
    }
    fill(0, 0, Nil).toMap.par
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

  // Apply a sequence of captures (pure). Validates each capture; returns None on invalid step.
  def applyMoveSequence(board: Board, player: Stone, startFrom: Coord2D, destinations: List[Coord2D], rows: Int, cols: Int, openCoords: List[Coord2D]): Option[(Board, List[Coord2D])] = {
    @tailrec
    def loop(currBoard: Board, currOpen: List[Coord2D], from: Coord2D, remaining: List[Coord2D]): Option[(Board, List[Coord2D])] = remaining match {
      case Nil => Some((currBoard, currOpen))
      case to :: tail =>
        if (!isValidMove(currBoard, player, from, to, rows, cols)) None
        else {
          val mid = ((from._1 + to._1) / 2, (from._2 + to._2) / 2)
          val (b2, o2) = executeMove(currBoard, player, from, to, mid, currOpen)
          loop(b2, o2, to, tail)
        }
    }

    loop(board, openCoords, startFrom, destinations)
  }

  // Continue a single capture decision purely: if choiceOpt is None, returns (board,open,false).
  // If Some(dest) validates dest and applies it, returning whether further captures are possible.
  def continueCapturePure(board: Board, player: Stone, lastPos: Coord2D, choiceOpt: Option[Coord2D], rows: Int, cols: Int, openCoords: List[Coord2D]): Option[(Board, List[Coord2D], Boolean)] = {
    choiceOpt match {
      case None => Some((board, openCoords, false))
      case Some(dest) =>
        val possible = getValidMovesForPiece(board, player, lastPos, rows, cols)
        if (!possible.contains(dest)) None
        else {
          val mid = ((lastPos._1 + dest._1) / 2, (lastPos._2 + dest._2) / 2)
          val (b2, o2) = executeMove(board, player, lastPos, dest, mid, openCoords)
          val canContinue = canMoveAgain(b2, player, dest, rows, cols)
          Some((b2, o2, canContinue))
        }
    }
  }


  //T2
  def play(board: Board, player: Stone,
           coordFrom: Coord2D, coordTo: Coord2D,
           lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = {
    val mid = ((coordFrom._1 + coordTo._1) / 2, (coordFrom._2 + coordTo._2) / 2)

    val rows = getRows(board)
    val cols = getCols(board)

    if (isValidMove(board, player, coordFrom, coordTo, rows, cols)) {
        val (newBoard, newOpen) = executeMove(board, player, coordFrom, coordTo, mid, lstOpenCoords)
        (Some(newBoard), newOpen)
    } else {
        (None, lstOpenCoords)
    }
  }

  def getAllValidMoves(board: Board, player: Stone, rows: Int, cols: Int): List[(Coord2D, Coord2D)] = {

    val allCoords = board.keys.toList

    @tailrec
    def loop(remaining: List[Coord2D], acc: List[(Coord2D, Coord2D)]): List[(Coord2D, Coord2D)] = remaining match {
      case Nil => acc.reverse
      case from :: tail =>
        val dests = getValidMovesForPiece(board, player, from, rows, cols)
        val pairs = dests.map(to => (from, to))
        loop(tail, pairs.reverse ++ acc)
    }

    loop(allCoords, Nil)
  }
  
  def getValidMovesForPiece(board: Board, player: Stone, from: Coord2D, rows: Int, cols: Int): List[Coord2D] = {
    val directions = List((2, 0), (-2, 0), (0, 2), (0, -2))

    @tailrec
    def loop(ds: List[(Int, Int)], acc: List[Coord2D]): List[Coord2D] = ds match {
      case Nil => acc.reverse
      case (dr, dc) :: tail =>
        val to = (from._1 + dr, from._2 + dc)
        if (isValidMove(board, player, from, to, rows, cols)) loop(tail, to :: acc)
        else loop(tail, acc)
    }

    loop(directions, Nil)
  }

  def canMoveAgain(board: Board, player: Stone, from: Coord2D, rows: Int, cols: Int): Boolean =
    getValidMovesForPiece(board, player, from, rows, cols).nonEmpty

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

    val colHeader = "    " + (0 until cols).toList.map(c => ('A'.toInt + c).toChar.toString).mkString("   ") + "\n"

    val separator = "  " + (0 until cols).toList.map(_ => "---").mkString("+", "+", "+\n")

    val boardBody = (0 until rows).toList.foldLeft("") ( (acc, r) =>
      val rowContent = r.toString + " | " + (0 until cols).toList.map { c =>
        board.get((r, c)) match {
          case Some(Stone.Black) => "B"
          case Some(Stone.White) => "W"
          case None              => " "
        }
      }.mkString(" | ") + " |\n"

      acc + rowContent + separator
    )
    "\n" + colHeader + separator + boardBody
  }

  // Serialization / parsing of saved game content (pure):
  def serializeGame(board: Board, randSeed: Long, currentPlayer: Stone, openCoords: List[Coord2D], rows: Int, cols: Int, mode: String, playerColorOpt: Option[Stone], difficulty: String): String = {
    val header = s"${rows},${cols},${currentPlayer},${randSeed},${mode},${playerColorOpt.map{case Stone.Black=>"Black"; case Stone.White=>"White"}.getOrElse("None") },${difficulty}"
    val boardLines = board.toList.map { case ((r, c), stone) =>
      val s = stone match { case Stone.Black => "B"; case Stone.White => "W" }
      s"${r},${c},${s}"
    }
    val openLines = openCoords.map { case (r, c) => s"${r},${c}" }
    (header :: "BOARD" :: boardLines ::: "OPEN" :: openLines).mkString("\n")
  }

  def parseGameContent(content: String): Option[(Board, MyRandom, Stone, List[Coord2D], Int, Int, String, Option[Stone], String)] = {
    try {
      val lines = content.split("\\r?\\n").toList
      if (lines.isEmpty) return None
      val header = lines.head.split(',').map(_.trim).toList
      // expected header: rows,cols,currentPlayer,randSeed,mode,playerColor,difficulty
      header match {
          case rStr :: cStr :: currentPlayerStr :: randSeedStr :: modeStr :: playerColorStr :: difficultyStr :: Nil =>
          val r = rStr.toInt
          val c = cStr.toInt
          val currentPlayer = if (currentPlayerStr == "Black") Stone.Black else Stone.White
          val randSeed = randSeedStr.toLong
          val mode = modeStr
          val playerColorOpt = playerColorStr match { case "Black" => Some(Stone.Black); case "White" => Some(Stone.White); case _ => None }
          val difficulty = difficultyStr

          val (_, boardListRev, openListRev) = lines.drop(1).foldLeft[(String, List[((Int, Int), Stone)], List[(Int, Int)])]( ("HEADER", List.empty[((Int, Int), Stone)], List.empty[(Int, Int)]) ) {
            case ((section, boardAcc, openAcc), line) if line == "BOARD" => ("BOARD", boardAcc, openAcc)
            case ((section, boardAcc, openAcc), line) if line == "OPEN" => ("OPEN", boardAcc, openAcc)
            case ((section, boardAcc, openAcc), line) if section == "BOARD" =>
              val parts = line.split(',').map(_.trim).toList
              parts match {
                case rStr2 :: cStr2 :: sStr :: Nil =>
                  val rr = rStr2.toInt
                  val cc = cStr2.toInt
                  val st = if (sStr == "B") Stone.Black else Stone.White
                  ("BOARD", ((rr, cc), st) :: boardAcc, openAcc)
                case _ => (section, boardAcc, openAcc)
              }
            case ((section, boardAcc, openAcc), line) if section == "OPEN" =>
              val parts = line.split(',').map(_.trim).toList
              parts match {
                case rStr2 :: cStr2 :: Nil => ("OPEN", boardAcc, (rStr2.toInt, cStr2.toInt) :: openAcc)
                case _ => (section, boardAcc, openAcc)
              }
            case ((section, boardAcc, openAcc), _) => (section, boardAcc, openAcc)
          }

          val board = boardListRev.reverse.toMap.par
          val openCoords = openListRev.reverse
          val rand = MyRandom(randSeed)
          Some((board, rand, currentPlayer, openCoords, r, c, mode, playerColorOpt, difficulty))
        case _ => None
      }
    } catch {
      case _: Exception => None
    }
  }

  // Accept only known time options (in milliseconds)
  val acceptedTimeOptions: Set[Long] = Set(10000L, 30000L, 60000L, 120000L, 300000L, 600000L)
  def isAcceptedTimeMillis(ms: Long): Boolean = acceptedTimeOptions.contains(ms)

  def parseInput(input: String): Option[Coord2D] = {
    val trimmed = input.trim
    if (trimmed.length < 2) None
    else {
      val colChar = trimmed.toUpperCase.head
      val rowPart = trimmed.tail

      scala.util.Try(rowPart.toInt) match {
        case scala.util.Success(row) if colChar >= 'A' && colChar <= 'Z' =>
          val col = colChar.toInt - 'A'.toInt
          Option((row, col))
        case _ => None
      }
    }
  }

  def coordToString(coord: Coord2D): String = {
    val colChar = (coord._2 + 'A'.toInt).toChar
    colChar.toString + coord._1.toString
  }

  def isValidDimension(r: Int, c: Int): Boolean =
    r > 2 && r <= 20 && c > 2 && c <= 20


  def getWinner(board: Board, currentPlayer: Stone, rows: Int, cols: Int): Option[Stone] = {
    val moves = getAllValidMoves(board, currentPlayer, rows, cols)
    moves match {
      case Nil =>
        if (currentPlayer == Stone.Black) Option(Stone.White) else Option(Stone.Black)
      case _ => None
    }
  }

}

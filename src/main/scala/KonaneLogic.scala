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

  private def getRows(board: Board): Int = if (board.isEmpty) 0 else board.keys.map(_._1).max + 1

  private def getCols(board: Board): Int = if (board.isEmpty) 0 else board.keys.map(_._2).max + 1

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

  private def executeMove(board: Board, player: Stone,
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

  private def canMoveAgain(board: Board, player: Stone, from: Coord2D, rows: Int, cols: Int): Boolean =
    getValidMovesForPiece(board, player, from, rows, cols).nonEmpty

  private def getDestinations(mvs: List[(Coord2D, Coord2D)]): List[Coord2D] = {

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
  // Serialise a single board snapshot (used for current state and each history entry).
  private def serializeSnapshot(b: Board, randSeed: Long, player: Stone, open: List[Coord2D]): List[String] = {
    val playerStr = player match { case Stone.Black => "Black"; case Stone.White => "White" }
    val meta = s"META,${randSeed},${playerStr}"
    val boardLines = b.toList.map { case ((r, c), stone) =>
      val s = stone match { case Stone.Black => "B"; case Stone.White => "W" }
      s"${r},${c},${s}"
    }
    val openLines = open.map { case (r, c) => s"${r},${c}" }
    meta :: "BOARD" :: boardLines ::: "OPEN" :: openLines
  }

  def serializeGame(board: Board, randSeed: Long, currentPlayer: Stone, openCoords: List[Coord2D], rows: Int, cols: Int, mode: String, playerColorOpt: Option[Stone], difficulty: String, history: List[(Board, MyRandom, Stone, List[Coord2D])]): String = {
    val header = s"${rows},${cols},${currentPlayer},${randSeed},${mode},${playerColorOpt.map{case Stone.Black=>"Black"; case Stone.White=>"White"}.getOrElse("None") },${difficulty}"
    val currentSnapshot = serializeSnapshot(board, randSeed, currentPlayer, openCoords)
    // History is stored oldest-first (head of list is most recent, so we reverse)
    val historySection = if (history.isEmpty) List("HISTORY", "END_HISTORY")
    else {
      val entryLines = history.reverse.foldLeft(List.empty[String]) { case (acc, (hb, hr, hp, ho)) =>
        acc ::: ("ENTRY" :: serializeSnapshot(hb, hr.seed, hp, ho))
      }
      "HISTORY" :: entryLines ::: List("END_HISTORY")
    }
    (header :: currentSnapshot ::: historySection).mkString("\n")
  }

  // Parse a board map from a list of "r,c,S" strings (accumulated in reverse).
  private def boardFromRevList(lst: List[((Int,Int), Stone)]): Board = lst.reverse.toMap.par

  // Parse open coords from a list of (r,c) pairs (accumulated in reverse).
  private def openFromRevList(lst: List[(Int,Int)]): List[Coord2D] = lst.reverse

  def parseGameContent(content: String): Option[(Board, MyRandom, Stone, List[Coord2D], Int, Int, String, Option[Stone], String, List[(Board, MyRandom, Stone, List[Coord2D])])] = {
    try {
      val rawLines = content.split("\\r?\\n").toList
      val lines = rawLines.map(_.trim).filter(_.nonEmpty)
      if (lines.isEmpty) return None

      val header = lines.head.split(',').map(_.trim).toList
      header match {
        case rStr :: cStr :: currentPlayerStr :: randSeedStr :: modeStr :: playerColorStr :: difficultyStr :: Nil =>
          val rows = rStr.toInt
          val cols = cStr.toInt
          val currentPlayer = if (currentPlayerStr == "Black") Stone.Black else Stone.White
          val randSeed = randSeedStr.toLong
          val mode = modeStr
          val playerColorOpt = playerColorStr match { case "Black" => Some(Stone.Black); case "White" => Some(Stone.White); case _ => None }
          val difficulty = difficultyStr

          // Helper to parse board cells from a slice of lines
          def parseBoardLines(bs: List[String]): List[((Int, Int), Stone)] =
            bs.flatMap { line =>
              val parts = line.split(',').map(_.trim).toList
              parts match {
                case rr :: cc :: sStr :: Nil =>
                  val st = if (sStr == "B") Stone.Black else Stone.White
                  Some(((rr.toInt, cc.toInt), st))
                case _ => None
              }
            }.reverse

          def parseOpenLines(os: List[String]): List[Coord2D] =
            os.flatMap { line =>
              val parts = line.split(',').map(_.trim).toList
              parts match { case rr :: cc :: Nil => Some((rr.toInt, cc.toInt)); case _ => None }
            }.reverse

          // Locate top-level BOARD and OPEN for current snapshot
          val boardIdx = lines.indexWhere(_ == "BOARD", 1)
          if (boardIdx < 0) return None
          val openIdx = lines.indexWhere(_ == "OPEN", boardIdx + 1)
          if (openIdx < 0) return None
          val boardLines = lines.slice(boardIdx + 1, openIdx)
          val openLines = {
            // find end of open section (HISTORY or end)
            val nextTagIdx = lines.indexWhere(l => l == "HISTORY" || l == "END_HISTORY", openIdx + 1)
            val endIdx = if (nextTagIdx < 0) lines.length else nextTagIdx
            lines.slice(openIdx + 1, endIdx)
          }

          val board = boardFromRevList(parseBoardLines(boardLines))
          val openCoords = parseOpenLines(openLines)
          val rand = MyRandom(randSeed)

          // Parse HISTORY section if present
          val historyStart = lines.indexWhere(_ == "HISTORY", openIdx + 1)
          val historyEntries: List[(Board, MyRandom, Stone, List[Coord2D])] =
            if (historyStart < 0) Nil
            else {
              // lines after HISTORY
              val after = lines.drop(historyStart + 1)

              // split into entry blocks between ENTRY markers and END_HISTORY
              @tailrec
              def splitEntries(rem: List[String], acc: List[List[String]]): List[List[String]] = rem match {
                case Nil => acc.reverse
                case "END_HISTORY" :: _ => acc.reverse
                case "ENTRY" :: tail => {
                  // collect until next ENTRY or END_HISTORY
                  @tailrec
                  def takeBlock(xs: List[String], collected: List[String]): (List[String], List[String]) = xs match {
                    case Nil => (collected.reverse, Nil)
                    case h :: t if h == "ENTRY" || h == "END_HISTORY" => (collected.reverse, xs)
                    case h :: t => takeBlock(t, h :: collected)
                  }
                  val (block, rest) = takeBlock(tail, Nil)
                  splitEntries(rest, block :: acc)
                }
                case _ :: tail => splitEntries(tail, acc)
              }

              val blocks = splitEntries(after, Nil)

              // parse one block into an entry
              def parseBlock(block: List[String]): Option[(Board, MyRandom, Stone, List[Coord2D])] = {
                if (block.isEmpty) return None
                // optional META at start
                val (metaOpt, rest1) = block match {
                  case h :: t if h.startsWith("META,") =>
                    val parts = h.split(',').map(_.trim).toList
                    parts match {
                      case _ :: seedStr :: playerStr :: Nil =>
                        val player = if (playerStr == "Black") Stone.Black else Stone.White
                        (Some((seedStr.toLong, player)), t)
                      case _ => (None, block)
                    }
                  case _ => (None, block)
                }

                val bIdx = rest1.indexWhere(_ == "BOARD")
                if (bIdx < 0) return None
                val oIdx = rest1.indexWhere(_ == "OPEN", bIdx + 1)
                if (oIdx < 0) return None
                val bLines = rest1.slice(bIdx + 1, oIdx)
                val oLines = rest1.slice(oIdx + 1, rest1.length)
                val b = boardFromRevList(parseBoardLines(bLines))
                val o = parseOpenLines(oLines)
                val (seed, player) = metaOpt.getOrElse((0L, Stone.Black))
                Some((b, MyRandom(seed), player, o))
              }

              blocks.flatMap(parseBlock)
            }

          // historyEntries is oldest-first (file order). Convert to newest-first (head is most recent)
          val historyNewestFirst = historyEntries.reverse

          Some((board, rand, currentPlayer, openCoords, rows, cols, mode, playerColorOpt, difficulty, historyNewestFirst))

        case _ => None
      }
    } catch {
      case _: Exception => None
    }
  }

  // Accept only known time options (in milliseconds)
  private val acceptedTimeOptions: Set[Long] = Set(10000L, 30000L, 60000L, 120000L, 300000L, 600000L)
  private def isAcceptedTimeMillis(ms: Long): Boolean = acceptedTimeOptions.contains(ms)

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
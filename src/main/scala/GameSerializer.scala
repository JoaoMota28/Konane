import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.ImmutableMapIsParallelizable

object GameSerializer {

    def serializeSnapshot(b: Board, randSeed: Long, player: Stone, open: List[Coord2D]): List[String] = {
        val playerStr = player match { case Stone.Black => "Black"; case Stone.White => "White" }
        val meta = s"META,${randSeed},${playerStr}"
        val boardLines = b.toList.map { case ((r, c), stone) =>
            val s = stone match { case Stone.Black => "B"; case Stone.White => "W" }
            s"${r},${c},${s}"
        }
        val openLines = open.map { case (r, c) => s"${r},${c}" }
        meta :: "BOARD" :: boardLines ::: "OPEN" :: openLines
    }

    def serializeGame(board: Board, randSeed: Long, currentPlayer: Stone, openCoords: List[Coord2D],
                      rows: Int, cols: Int, mode: String, playerColorOpt: Option[Stone],
                      difficulty: String, history: List[(Board, MyRandom, Stone, List[Coord2D])]): String = {
        val header = s"${rows},${cols},${currentPlayer},${randSeed},${mode},${playerColorOpt.map { case Stone.Black => "Black"; case Stone.White => "White" }.getOrElse("None")},${difficulty}"
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

    def boardFromLines(lst: List[((Int, Int), Stone)]): Board = lst.reverse.toMap.par

    def parseGameContent(content: String): Option[(Board, MyRandom, Stone, List[Coord2D], Int, Int, String, Option[Stone], String, List[(Board, MyRandom, Stone, List[Coord2D])])] = {
        try {
            val lines = content.split("\\r?\\n").toList.map(_.trim).filter(_.nonEmpty)
            if (lines.isEmpty) return None

            lines.head.split(',').map(_.trim).toList match {
                case rStr :: cStr :: currentPlayerStr :: randSeedStr :: modeStr :: playerColorStr :: difficultyStr :: Nil =>
                    val rows         = rStr.toInt
                    val cols         = cStr.toInt
                    val currentPlayer = if (currentPlayerStr == "Black") Stone.Black else Stone.White
                    val randSeed     = randSeedStr.toLong
                    val playerColorOpt = playerColorStr match {
                        case "Black" => Some(Stone.Black)
                        case "White" => Some(Stone.White)
                        case _       => None
                    }

                    val boardIdx = lines.indexWhere(_ == "BOARD", 1)
                    if (boardIdx < 0) return None
                    val openIdx = lines.indexWhere(_ == "OPEN", boardIdx + 1)
                    if (openIdx < 0) return None
                    val boardLines = lines.slice(boardIdx + 1, openIdx)
                    val openEndIdx = {
                        val next = lines.indexWhere(l => l == "HISTORY" || l == "END_HISTORY", openIdx + 1)
                        if (next < 0) lines.length else next
                    }
                    val openLines = lines.slice(openIdx + 1, openEndIdx)

                    val board      = boardFromLines(parseBoardLines(boardLines))
                    val openCoords = parseOpenLines(openLines)
                    val rand       = MyRandom(randSeed)


                    val historyStart = lines.indexWhere(_ == "HISTORY", openIdx + 1)
                    val historyEntries: List[(Board, MyRandom, Stone, List[Coord2D])] =
                        if (historyStart < 0) Nil
                        else {
                            val after = lines.drop(historyStart + 1)

                            @tailrec
                            def splitEntries(rem: List[String], acc: List[List[String]]): List[List[String]] = rem match {
                                case Nil | "END_HISTORY" :: _ => acc.reverse
                                case "ENTRY" :: tail =>
                                    @tailrec
                                    def takeBlock(xs: List[String], collected: List[String]): (List[String], List[String]) = xs match {
                                        case Nil                                          => (collected.reverse, Nil)
                                        case h :: _ if h == "ENTRY" || h == "END_HISTORY" => (collected.reverse, xs)
                                        case h :: t                                       => takeBlock(t, h :: collected)
                                    }
                                    val (block, rest) = takeBlock(tail, Nil)
                                    splitEntries(rest, block :: acc)
                                case _ :: tail => splitEntries(tail, acc)
                            }

                            def parseBlock(block: List[String]): Option[(Board, MyRandom, Stone, List[Coord2D])] = {
                                if (block.isEmpty) return None
                                val (metaOpt, rest1) = block match {
                                    case h :: t if h.startsWith("META,") =>
                                        h.split(',').map(_.trim).toList match {
                                            case _ :: seedStr :: playerStr :: Nil =>
                                                val p = if (playerStr == "Black") Stone.Black else Stone.White
                                                (Some((seedStr.toLong, p)), t)
                                            case _ => (None, block)
                                        }
                                    case _ => (None, block)
                                }
                                val bIdx = rest1.indexWhere(_ == "BOARD")
                                if (bIdx < 0) return None
                                val oIdx = rest1.indexWhere(_ == "OPEN", bIdx + 1)
                                if (oIdx < 0) return None
                                val b = boardFromLines(parseBoardLines(rest1.slice(bIdx + 1, oIdx)))
                                val o = parseOpenLines(rest1.slice(oIdx + 1, rest1.length))
                                val (seed, player) = metaOpt.getOrElse((0L, Stone.Black))
                                Some((b, MyRandom(seed), player, o))
                            }

                            splitEntries(after, Nil).flatMap(parseBlock)
                        }

                    Some((board, rand, currentPlayer, openCoords, rows, cols, modeStr, playerColorOpt, difficultyStr, historyEntries.reverse))

                case _ => None
            }
        } catch {
            case _: Exception => None
        }
    }
}
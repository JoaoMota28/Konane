import java.io.{File, PrintWriter}
import scala.io.Source
import KonaneLogic.*

object SaveLoadGame {

  private val savePath = "saves/konane_save.txt"

  def saveGame(
    board: Board,
    currentPlayer: Stone,
    openCoords: List[Coord2D],
    rows: Int,
    cols: Int,
    mode: String,
    playerColorOpt: Option[Stone],
    difficulty: String
  ): Unit = {
    try {
      val dir = new File("saves")
      if (!dir.exists()) dir.mkdir()
      val content = KonaneLogic.serializeGame(board, System.currentTimeMillis(), currentPlayer, openCoords, rows, cols, mode, playerColorOpt, difficulty)
      val pw = new PrintWriter(new File(savePath))
      pw.write(content)
      pw.close()
      println(s"Jogo salvo em: $savePath")
    } catch {
      case e: Exception => println("Erro ao salvar: " + e.getMessage)
    }
  }

  def loadGame(): Option[(Board, MyRandom, Stone, List[Coord2D], Int, Int, String, Option[Stone], String)] = {
    val file = new File(savePath)
    if (!file.exists()) return None
    try {
      val source = Source.fromFile(file)
      val content = source.mkString
      source.close()
      KonaneLogic.parseGameContent(content)
    } catch {
      case _: Exception => None
    }
  }
}


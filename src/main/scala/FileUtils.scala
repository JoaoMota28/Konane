import scala.util.Using

object FileUtils {
  def deleteIfExists(path: String): Unit = {
    try {
      val f = new java.io.File(path)
      if (f.exists()) { f.delete(); println("Ficheiro de salvamento eliminado: " + path) }
    } catch { case _: Exception => () }
  }

  def fileNameExists(fileName: String): Boolean = {
    new java.io.File(s"saves/${fileName}.txt").exists()
  }

  def saveGame(fileName: String, board: Board, rand: MyRandom, currentPlayer: Stone, openCoords: List[Coord2D], rows: Int, cols: Int, mode: String, playerColor: Option[Stone], difficulty: String): Boolean = {
    try {
      val dir = new java.io.File("saves")
      if (!dir.exists()) dir.mkdir()
      val file = new java.io.File(dir, s"${fileName}.txt")
      val pw = new java.io.PrintWriter(file)
      val content = KonaneLogic.serializeGame(board, rand.seed, currentPlayer, openCoords, rows, cols, mode, playerColor, difficulty)
      pw.println(content)
      pw.close()
      println(s"Jogo salvo em: ${file.getPath}")
      true
    } catch {
      case e: Exception => println("Erro ao salvar jogo: " + e.getMessage); false
    }
  }

  def loadGameFromFile(path: String): Option[(Board, MyRandom, Stone, List[Coord2D], Int, Int, String, Option[Stone], String)] = {
    // read file content safely and close source
    val contentTry = Using(scala.io.Source.fromFile(path)) { src => src.mkString }
    contentTry.toOption match {
      case Some(content) => KonaneLogic.parseGameContent(content)
      case None => println("Erro ao ler ficheiro: ficheiro inacessivel ou formato invalido."); None
    }
  }
}



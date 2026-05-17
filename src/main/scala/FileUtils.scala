import scala.util.Using

object FileUtils {

    def fileNameExists(fileName: String): Boolean =
        new java.io.File(s"saves/${fileName}.txt").exists()

    def saveGame(fileName: String, board: Board, rand: MyRandom, currentPlayer: Stone,
                 openCoords: List[Coord2D], rows: Int, cols: Int, mode: String,
                 playerColor: Option[Stone], difficulty: String,
                 history: List[(Board, MyRandom, Stone, List[Coord2D])]): Boolean =
        try {
            val dir  = new java.io.File("saves")
            if (!dir.exists()) dir.mkdir()
            val file = new java.io.File(dir, s"${fileName}.txt")
            val pw   = new java.io.PrintWriter(file)
            pw.println(GameSerializer.serializeGame(board, rand.seed, currentPlayer, openCoords,
                rows, cols, mode, playerColor, difficulty, history))
            pw.close()
            println(s"Jogo salvo em: ${file.getPath}")
            true
        } catch {
            case e: Exception => println("Erro ao salvar jogo: " + e.getMessage); false
        }

    def loadGameFromFile(path: String): Option[(Board, MyRandom, Stone, List[Coord2D], Int, Int, String, Option[Stone], String, List[(Board, MyRandom, Stone, List[Coord2D])])] =
        Using(scala.io.Source.fromFile(path))(_.mkString).toOption match {
            case Some(content) => GameSerializer.parseGameContent(content)
            case None          => println("Erro ao ler ficheiro: ficheiro inacessivel ou formato invalido."); None
        }
}
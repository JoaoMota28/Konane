import scala.annotation.tailrec

object InputHelper {
  @tailrec
  def readNonEmptyTrimmed(): String = {
    val s = scala.io.StdIn.readLine()
    if (s == null) readNonEmptyTrimmed()
    else {
      val t = s.trim
      if (t.isEmpty) readNonEmptyTrimmed() else t
    }
  }

  @tailrec
  def readYesNo(prompt: String): Boolean = {
    print(prompt)
    val inRaw = scala.io.StdIn.readLine()
    val in = if (inRaw == null) "" else inRaw.trim.toLowerCase
    if (in == "exit") throw new RuntimeException("EXIT")
    in match {
      case "" => readYesNo(prompt)
      case "y" => true
      case "n" => false
      case _ =>
        println("Resposta invalida. Responda 'y' ou 'n'.")
        readYesNo(prompt)
    }
  }
}


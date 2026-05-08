import scala.annotation.tailrec
// No concurrency: per-move timeout is enforced by checking elapsed time after each blocking read

object KonaneTUI {
  import GameEngine.*
  import InputHelper.*
  import FileUtils.*

  def main(args: Array[String]): Unit = menuLoop(8, 8, 60000L, "facil")

  @tailrec
  private def menuLoop(rows: Int, cols: Int, timeLimitMillis: Long, difficulty: String): Unit = {
    println("--- KONANE MENU ---")
    println("1) Jogar contra o Computador")
    println("2) Jogar contra outro Jogador")
    println("3) Carregar Jogo")
    println("4) Opcoes")
    println("5) Sair")
    print("Escolha: ")
    readNonEmptyTrimmed() match {
      case "1" => handlePlayVsComputer(rows, cols, timeLimitMillis, difficulty)
      case "2" => handlePlayHvH(rows, cols, timeLimitMillis, difficulty)
      case "3" => handleLoadGame(rows, cols, timeLimitMillis, difficulty)
      case "4" => handleOptions(rows, cols, timeLimitMillis, difficulty)
      case "5" => println("Saindo...")
      case _ => println("Opcao invalida."); menuLoop(rows, cols, timeLimitMillis, difficulty)
    }
  }

  @scala.annotation.tailrec
  private def timedReadNonEmptyTrimmed(startTime: Long, timeLimitMillis: Long): Option[String] = {
    // Blocking read; after each read check elapsed time. This enforces the move timer without concurrency.
    val elapsedBefore = System.currentTimeMillis() - startTime
    if (elapsedBefore > timeLimitMillis) return None
    val s = scala.io.StdIn.readLine()
    val elapsedAfter = System.currentTimeMillis() - startTime
    if (elapsedAfter > timeLimitMillis) return None
    if (s == null) timedReadNonEmptyTrimmed(startTime, timeLimitMillis)
    else {
      val t = s.trim
      if (t.isEmpty) timedReadNonEmptyTrimmed(startTime, timeLimitMillis) else Some(t)
    }
  }

  private def handleLoadGame(rows: Int, cols: Int, timeLimitMillis: Long, difficulty: String): Unit = {
    val dir = new java.io.File("saves")
    if (!dir.exists() || dir.listFiles().isEmpty) {
      println("Nenhum ficheiro de salvamento disponivel.")
      menuLoop(rows, cols, timeLimitMillis, difficulty)
    } else {
      val files = dir.listFiles().filter(_.getName.endsWith(".txt")).toList
      println("Ficheiros de salvamento:")
      @tailrec
      def printFiles(fs: List[java.io.File], idx: Int): Unit = fs match {
        case Nil => ()
        case h :: t => println(s"${idx}) ${h.getName}"); printFiles(t, idx + 1)
      }
      printFiles(files, 1)
      print("Escolha o numero do ficheiro para carregar: ")
      val fileChoiceStr = readNonEmptyTrimmed()
      scala.util.Try(fileChoiceStr.toInt).toOption match {
        case Some(n) if n >= 1 && n <= files.length =>
          val f = files(n - 1)
          FileUtils.loadGameFromFile(f.getPath) match {
            case Some((board, rand, currentPlayer, openCoords, r, c, mode, playerColorOpt, savedDifficulty)) =>
              println("--- JOGO CARREGADO ---")
              val (blackH, whiteH) = mode match {
                case "HVC" => playerColorOpt match { case Some(Stone.Black) => (true, false); case Some(Stone.White) => (false, true); case _ => (true, false) }
                case "HVH" => (true, true)
                case _ => (true, false)
              }
              val loadedState = GameState(board = board, rand = rand, currentPlayer = currentPlayer, openCoords = openCoords, rows = r, cols = c, history = Nil, blackPlayer = blackH, whitePlayer = whiteH, mode = mode, playerColorOpt = playerColorOpt, loadedSavePath = Some(f.getPath), timeLimitMillis = timeLimitMillis, difficulty = savedDifficulty, pendingCapture = None)
              gameLoopState(loadedState)
            case None => println("Erro ao carregar ficheiro."); menuLoop(rows, cols, timeLimitMillis, difficulty)
          }
        case _ => println("Selecao invalida."); menuLoop(rows, cols, timeLimitMillis, difficulty)
      }
    }
  }

  private def handlePlayVsComputer(rows: Int, cols: Int, timeLimitMillis: Long, difficulty: String): Unit = {
    println("Escolha sua cor: 1) Preto 2) Branco 3) Aleatorio")
    print("Opcao: ")
    val initSeed = MyRandom(System.currentTimeMillis())
    readNonEmptyTrimmed() match {
      case "1" => startNewGame(rows, cols, playerPlaysBlack = true, initSeed, timeLimitMillis, difficulty)
      case "2" => startNewGame(rows, cols, playerPlaysBlack = false, initSeed, timeLimitMillis, difficulty)
      case "3" => val (choice, seedAfter) = initSeed.nextInt(2); startNewGame(rows, cols, playerPlaysBlack = choice == 0, seedAfter, timeLimitMillis, difficulty)
      case _ => println("Opcao invalida."); menuLoop(rows, cols, timeLimitMillis, difficulty)
    }
  }

  private def handlePlayHvH(rows: Int, cols: Int, timeLimitMillis: Long, difficulty: String): Unit = {
    val seed = MyRandom(System.currentTimeMillis())
    val (boardReady, openCoords, randAfterSetup) = GameEngine.setupBoard(GameEngine.initBoard(rows, cols), rows, cols, seed)
    println("--- JOGO INICIADO (Player vs Player) ---")
    val initialState = GameState(board = boardReady, rand = randAfterSetup, currentPlayer = Stone.Black, openCoords = openCoords, rows = rows, cols = cols, history = Nil, blackPlayer = true, whitePlayer = true, mode = "HVH", playerColorOpt = None, loadedSavePath = None, timeLimitMillis = timeLimitMillis, difficulty = difficulty, pendingCapture = None)
    gameLoopState(initialState)
  }

  private def handleOptions(rows: Int, cols: Int, timeLimitMillis: Long, difficulty: String): Unit = {
    println("--- OPCOES ---")
    println(s"Tamanho atual: ${rows}x${cols}")
    println(s"Tempo por jogada atual: ${formatMillis(timeLimitMillis)}")
    println("1) Alterar dimensoes")
    println("2) Alterar tempo por jogada")
    println("3) Alterar dificuldade (atual: " + difficulty + ")")
    println("4) Voltar")
    print("Opcao: ")
    readNonEmptyTrimmed() match {
      case "1" =>
        print("Novo numero de linhas: ")
        val nrOpt = scala.util.Try(readNonEmptyTrimmed().toInt).toOption
        print("Novo numero de colunas: ")
        val ncOpt = scala.util.Try(readNonEmptyTrimmed().toInt).toOption
        (nrOpt, ncOpt) match {
          case (Some(nr), Some(nc)) if isValidDimension(nr, nc) => println("Dimensoes alteradas."); menuLoop(nr, nc, timeLimitMillis, difficulty)
          case _ => println("Dimensoes invalidas. Devem estar entre 3 e 20."); menuLoop(rows, cols, timeLimitMillis, difficulty)
        }

      case "2" =>
        println("Escolha novo tempo por jogada:")
        println("1) 10 segundos\n2) 30 segundos\n3) 1 minuto\n4) 2 minutos\n5) 5 minutos\n6) 10 minutos\n(pressione Enter para manter atual)")
        print("Opcao: ")
        val tChoice = scala.io.StdIn.readLine().trim
        val newTime = tChoice match {
          case "1" => 10 * 1000L
          case "2" => 30 * 1000L
          case "3" => 60 * 1000L
          case "4" => 2 * 60 * 1000L
          case "5" => 5 * 60 * 1000L
          case "6" => 10 * 60 * 1000L
          case _ => timeLimitMillis
        }
        println("Tempo atualizado.")
        menuLoop(rows, cols, newTime, difficulty)

      case "3" =>
        println("Escolha dificuldade:")
        println("1) Facil")
        println("2) Medio")
        println("3) Dificil")
        print("Opcao: ")
        readNonEmptyTrimmed() match {
          case "1" => println("Dificuldade alterada para facil."); menuLoop(rows, cols, timeLimitMillis, "facil")
          case "2" => println("Dificuldade alterada para medio."); menuLoop(rows, cols, timeLimitMillis, "medio")
          case "3" => println("Dificuldade alterada para dificil."); menuLoop(rows, cols, timeLimitMillis, "dificil")
          case _ => println("Opcao invalida."); menuLoop(rows, cols, timeLimitMillis, difficulty)
        }

      case _ => // back or invalid
        menuLoop(rows, cols, timeLimitMillis, difficulty)
    }
  }

  private def formatMillis(ms: Long): String = ms match {
    case 10000L => "10s"
    case 30000L => "30s"
    case 60000L => "1m"
    case 120000L => "2m"
    case 300000L => "5m"
    case 600000L => "10m"
    case other => s"${other / 1000} seconds"
  }

  private def startNewGame(rows: Int, cols: Int, playerPlaysBlack: Boolean, seed: MyRandom, timeLimitMillis: Long, difficulty: String): Unit = {
    val (boardReady, openCoords, randAfterSetup) = GameEngine.setupBoard(GameEngine.initBoard(rows, cols), rows, cols, seed)
    val playerColor = if (playerPlaysBlack) Some(Stone.Black) else Some(Stone.White)
    val (blackH, whiteH) = if (playerPlaysBlack) (true, false) else (false, true)
    val mode = "HVC"
    println("--- JOGO INICIADO ---")
    val initialState = GameState(
      board = boardReady,
      rand = randAfterSetup,
      currentPlayer = Stone.Black,
      openCoords = openCoords,
      rows = rows,
      cols = cols,
      history = Nil,
      blackPlayer = blackH,
      whitePlayer = whiteH,
      mode = mode,
      playerColorOpt = playerColor,
      loadedSavePath = None,
      timeLimitMillis = timeLimitMillis,
      difficulty = difficulty,
      pendingCapture = None
    )
    gameLoopState(initialState)
  }

  // consolidated helpers to remove repeated code
  private def handleTimeout(state: GameState): Unit = {
    println("Tempo esgotado! Jogador perdeu por tempo.")
    state.loadedSavePath.foreach(FileUtils.deleteIfExists)
    menuLoop(state.rows, state.cols, state.timeLimitMillis, state.difficulty)
  }

  private def getCaptureChoiceResult(startTime: Long, stateForTimeout: GameState, captureState: GameState, opts: List[Coord2D]): Option[(GameState, TurnResult)] = {
    println("Escolha captura: ")
    opts.zipWithIndex.foreach{ case (o,i) => println(s"${i+1}) ${coordToString(o)}") }
    timedReadNonEmptyTrimmed(startTime, stateForTimeout.timeLimitMillis) match {
      case None => None
      case Some(choiceStr) =>
        val choiceIdx = scala.util.Try(choiceStr.toInt).toOption.getOrElse(1)
        val choice = opts(math.max(0, math.min(opts.length-1, choiceIdx-1)))
               val (ns2, res2) = GameEngine.handleAction(captureState, ContinueCapture(choice))
        Some((ns2, res2))
    }
  }

  def gameLoopState(initial: GameState): Unit = {
    @scala.annotation.tailrec
    def loop(state: GameState): Unit = {
      println(GameEngine.boardToString(state.board, state.rows, state.cols))

      KonaneLogic.getWinner(state.board, state.currentPlayer, state.rows, state.cols) match {
        case Some(winner) =>
          handleGameWin(winner, state.loadedSavePath)
        case None =>
          // decide who moves: human or computer
          val isHuman = (state.currentPlayer == Stone.Black && state.blackPlayer) || (state.currentPlayer == Stone.White && state.whitePlayer)
          if (isHuman) {
            // per-move timer: start and use timed reads
            val startTime = System.currentTimeMillis()
            print(s"Peca a mover (ou 'undo'/'save'/'random'/'exit'${if (state.mode=="HVC") ", 'restart'" else ""}): ")
            timedReadNonEmptyTrimmed(startTime, state.timeLimitMillis) match {
              case None => handleTimeout(state)
              case Some(fromStr) =>
                val cmd = fromStr.trim.toLowerCase
                cmd match {
                  case "undo" =>
                    GameEngine.handleAction(state, Undo) match {
                      case (ns, InvalidAction(msg)) => println(msg); loop(ns)
                      case (ns, _) => loop(ns)
                    }
                  case "save" =>
                    GameEngine.handleAction(state, Save) match {
                      case (ns, SaveRequested(b, r, cp, open, rr, cc, mode, pcol, diff)) =>
                        val ok = FileUtils.saveGame(b, r, cp, open, rr, cc, mode, pcol, diff)
                        if (!ok) println("Falha ao salvar. Tente novamente.")
                        loop(ns)
                      case (ns, _) => println("Nao foi possivel preparar save."); loop(ns)
                    }
                  case "random" =>
                    GameEngine.handleAction(state, RandomMove) match {
                      case (ns, InvalidAction(msg)) => println(msg); loop(ns)
                      case (s, CaptureRequired(sState, last, opts)) =>
                        getCaptureChoiceResult(startTime, state, sState, opts) match {
                          case None => handleTimeout(state)
                          case Some((ns2, res2)) => res2 match { case InvalidAction(m) => println(m); loop(ns2); case _ => loop(ns2) }
                        }
                      case (ns, MoveOk(s2)) => loop(s2)
                      case (ns, _) => loop(ns)
                    }
                  case "exit" => println("Voltando ao menu..."); menuLoop(state.rows, state.cols, state.timeLimitMillis, state.difficulty)
                  case other =>
                    // treat as from coord
                    KonaneLogic.parseInput(other) match {
                      case Some(from) =>
                        print("Para onde: ")
                        timedReadNonEmptyTrimmed(startTime, state.timeLimitMillis) match {
                          case None => handleTimeout(state)
                          case Some(toStr) =>
                            KonaneLogic.parseInput(toStr) match {
                              case Some(to) =>
                                             GameEngine.handleAction(state, MakeMove(from, to)) match {
                                               case (ns, InvalidAction(msg)) => println(msg); loop(ns)
                                               case (s, CaptureRequired(sState, last, opts)) =>
                                                 getCaptureChoiceResult(startTime, state, sState, opts) match {
                                                   case None => handleTimeout(state)
                                                   case Some((ns2, res2)) => res2 match { case InvalidAction(m) => println(m); loop(ns2); case _ => loop(ns2) }
                                                 }
                                               case (ns, MoveOk(s2)) => loop(s2)
                                               case (ns, _) => loop(ns)
                                             }
                              case None => println("Entrada invalida para destino."); loop(state)
                            }
                        }
                      case None => println("Entrada invalida."); loop(state)
                    }
                }
            }
          } else {
            // computer move
            println("Vez do Computador...")
             GameEngine.handleAction(state, RandomMove) match {
              case (ns, InvalidAction(msg)) => println(msg); loop(ns)
              case (s, CaptureRequired(sState, last, opts)) =>
                // for computer, pick first possible
                val choice = opts.head
                 val (ns2, res2) = GameEngine.handleAction(sState, ContinueCapture(choice))
                res2 match { case InvalidAction(m) => println(m); loop(ns2); case _ => loop(ns2) }
              case (ns, MoveOk(s2)) => loop(s2)
              case (ns, _) => loop(ns)
            }
          }
      }
    }
    loop(initial)
  }

  private def handleGameWin(winner: Stone, loadedSavePath: Option[String]): Unit = {
    val winnerName = if (winner == Stone.Black) "Pretas (B)" else "Brancas (W)"
    println("--- FIM DE JOGO ---")
    println("Vencedor: " + winnerName)
    loadedSavePath match {
      case Some(path) => FileUtils.deleteIfExists(path)
      case None => ()
    }
  }


}



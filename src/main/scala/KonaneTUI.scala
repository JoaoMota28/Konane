import scala.annotation.tailrec

object KonaneTUI {

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
            case ""  => readYesNo(prompt)
            case "y" => true
            case "n" => false
            case _ =>
                println("Resposta invalida. Responda 'y' ou 'n'.")
                readYesNo(prompt)
        }
    }

    def isHumanTurn(state: GameState): Boolean = state.mode match {
        case "HVH" => true
        case "HVC" => state.playerColorOpt.contains(state.currentPlayer)
        case _     => false
    }

    def main(args: Array[String]): Unit = menuLoop(8, 8, 60000L, "facil")

    @tailrec
    def menuLoop(rows: Int, cols: Int, timeLimitMillis: Long, difficulty: String): Unit = {
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
            case _   => println("Opcao invalida."); menuLoop(rows, cols, timeLimitMillis, difficulty)
        }
    }

    @tailrec
    def timedReadNonEmptyTrimmed(startTime: Long, timeLimitMillis: Long): Option[String] = {
        if (System.currentTimeMillis() - startTime > timeLimitMillis) return None
        val s = scala.io.StdIn.readLine()
        if (System.currentTimeMillis() - startTime > timeLimitMillis) return None
        if (s == null) timedReadNonEmptyTrimmed(startTime, timeLimitMillis)
        else {
            val t = s.trim
            if (t.isEmpty) timedReadNonEmptyTrimmed(startTime, timeLimitMillis) else Some(t)
        }
    }

    def handleLoadGame(rows: Int, cols: Int, timeLimitMillis: Long, difficulty: String): Unit = {
        val dir     = new java.io.File("saves")
        val listOpt = Option(dir.listFiles())
        if (!dir.exists() || listOpt.isEmpty || listOpt.get.isEmpty) {
            println("Nenhum ficheiro disponível.")
            menuLoop(rows, cols, timeLimitMillis, difficulty)
        } else {
            val files = listOpt.get.filter(_.getName.endsWith(".txt")).toList
            println("Ficheiros disponíveis:")
            
            @tailrec
            def printFiles(fs: List[java.io.File], idx: Int): Unit = fs match {
                case Nil     => ()
                case h :: t  => println(s"${idx}) ${h.getName}"); printFiles(t, idx + 1)
            }
            printFiles(files, 1)
            print("Escolha o numero do ficheiro para carregar: ")
            scala.util.Try(readNonEmptyTrimmed().toInt).toOption match {
                case Some(n) if n >= 1 && n <= files.length =>
                    FileUtils.loadGameFromFile(files(n - 1).getPath) match {
                        case Some((board, rand, currentPlayer, openCoords, r, c, mode, playerColorOpt, savedDifficulty, savedHistory)) =>
                            println("--- JOGO CARREGADO ---")
                            val loadedState = GameState(board = board, rand = rand, currentPlayer = currentPlayer,
                                openCoords = openCoords, rows = r, cols = c, history = savedHistory, mode = mode,
                                playerColorOpt = playerColorOpt, timeLimitMillis = timeLimitMillis,
                                difficulty = savedDifficulty, pendingCapture = None)
                            gameLoopState(loadedState)
                        case None =>
                            println("Erro ao carregar ficheiro."); menuLoop(rows, cols, timeLimitMillis, difficulty)
                    }
                case _ => println("Selecao invalida."); menuLoop(rows, cols, timeLimitMillis, difficulty)
            }
        }
    }

    def handlePlayVsComputer(rows: Int, cols: Int, timeLimitMillis: Long, difficulty: String): Unit = {
        println("Escolha sua cor: 1) Preto 2) Branco 3) Aleatorio")
        print("Opcao: ")
        val initSeed = MyRandom(System.currentTimeMillis())
        readNonEmptyTrimmed() match {
            case "1" => startNewGame(rows, cols, playerPlaysBlack = true, initSeed, timeLimitMillis, difficulty)
            case "2" => startNewGame(rows, cols, playerPlaysBlack = false, initSeed, timeLimitMillis, difficulty)
            case "3" =>
                val (choice, seedAfter) = initSeed.nextInt(2)
                startNewGame(rows, cols, playerPlaysBlack = choice == 0, seedAfter, timeLimitMillis, difficulty)
            case _ => println("Opcao invalida."); menuLoop(rows, cols, timeLimitMillis, difficulty)
        }
    }

    def handlePlayHvH(rows: Int, cols: Int, timeLimitMillis: Long, difficulty: String): Unit = {
        val seed = MyRandom(System.currentTimeMillis())
        val (boardReady, openCoords, randAfterSetup) =
            KonaneLogic.setupBoard(KonaneLogic.initBoard(rows, cols), rows, cols, seed)
        println("--- JOGO INICIADO (Player vs Player) ---")
        val initialState = GameState(board = boardReady, rand = randAfterSetup, currentPlayer = Stone.Black,
            openCoords = openCoords, rows = rows, cols = cols, history = Nil, mode = "HVH",
            playerColorOpt = None, timeLimitMillis = timeLimitMillis, difficulty = difficulty, pendingCapture = None)
        gameLoopState(initialState)
    }

    def handleOptions(rows: Int, cols: Int, timeLimitMillis: Long, difficulty: String): Unit = {
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
                    case (Some(nr), Some(nc)) if KonaneLogic.isValidDimension(nr, nc) =>
                        println("Dimensoes alteradas."); menuLoop(nr, nc, timeLimitMillis, difficulty)
                    case _ =>
                        println("Dimensoes invalidas. Devem estar entre 3 e 20."); menuLoop(rows, cols, timeLimitMillis, difficulty)
                }
            case "2" =>
                println("Escolha novo tempo por jogada:")
                println("1) 10 segundos\n2) 30 segundos\n3) 1 minuto\n4) 2 minutos\n5) 5 minutos\n6) 10 minutos\n(pressione Enter para manter atual)")
                print("Opcao: ")
                val newTime = scala.io.StdIn.readLine().trim match {
                    case "1" => 10000L
                    case "2" => 30000L
                    case "3" => 60000L
                    case "4" => 120000L
                    case "5" => 300000L
                    case "6" => 600000L
                    case _   => timeLimitMillis
                }
                println("Tempo atualizado.")
                menuLoop(rows, cols, newTime, difficulty)
            case "3" =>
                println("Escolha dificuldade:")
                println("1) Facil\n2) Medio\n3) Dificil")
                print("Opcao: ")
                readNonEmptyTrimmed() match {
                    case "1" => println("Dificuldade alterada para facil.");  menuLoop(rows, cols, timeLimitMillis, "facil")
                    case "2" => println("Dificuldade alterada para medio.");  menuLoop(rows, cols, timeLimitMillis, "medio")
                    case "3" => println("Dificuldade alterada para dificil."); menuLoop(rows, cols, timeLimitMillis, "dificil")
                    case _   => println("Opcao invalida."); menuLoop(rows, cols, timeLimitMillis, difficulty)
                }
            case _ => menuLoop(rows, cols, timeLimitMillis, difficulty)
        }
    }

    def formatMillis(ms: Long): String = ms match {
        case 10000L  => "10s"
        case 30000L  => "30s"
        case 60000L  => "1m"
        case 120000L => "2m"
        case 300000L => "5m"
        case 600000L => "10m"
        case other   => s"${other / 1000} seconds"
    }

    def startNewGame(rows: Int, cols: Int, playerPlaysBlack: Boolean, seed: MyRandom,
                     timeLimitMillis: Long, difficulty: String): Unit = {
        val (boardReady, openCoords, randAfterSetup) =
            KonaneLogic.setupBoard(KonaneLogic.initBoard(rows, cols), rows, cols, seed)
        println("--- JOGO INICIADO ---")
        val initialState = GameState(
            board = boardReady, rand = randAfterSetup, currentPlayer = Stone.Black,
            openCoords = openCoords, rows = rows, cols = cols, history = Nil, mode = "HVC",
            playerColorOpt = Some(if (playerPlaysBlack) Stone.Black else Stone.White),
            timeLimitMillis = timeLimitMillis, difficulty = difficulty, pendingCapture = None
        )
        gameLoopState(initialState)
    }

    def handleTimeout(state: GameState): Unit = {
        println("Tempo esgotado! Jogador perdeu por tempo.")
        menuLoop(state.rows, state.cols, state.timeLimitMillis, state.difficulty)
    }

    def shouldContinueCaptureChain(difficulty: String): Boolean =
        difficulty.toLowerCase match {
            case "facil" => false
            case _       => true
        }

    @tailrec
    def printCaptureOptions(opts: List[(Int, Int)], idx: Int = 1): Unit = opts match {
        case Nil     => ()
        case h :: t  => println(s"$idx) ${KonaneLogic.coordToString(h)}"); printCaptureOptions(t, idx + 1)
    }

    @tailrec
    def handleCaptureChain(isHuman: Boolean, sState: GameState, opts: List[(Int, Int)],
                           state: GameState, startTime: Long): GameState = {
        if (isHuman) {
            println(KonaneLogic.boardToString(sState.board, sState.rows, sState.cols))
            println("Capturaste uma peça! Queres continuar a capturar com esta peça? (s/n)")
            timedReadNonEmptyTrimmed(startTime, state.timeLimitMillis) match {
                case None =>
                    handleTimeout(state); state
                case Some(response) =>
                    response.trim.toLowerCase match {
                        case "s" =>
                            println("Escolha o destino da captura: ")
                            printCaptureOptions(opts)
                            readAndProcessCaptureChoice(sState, opts, state, startTime)
                        case "n" =>
                            GameEngine.handleAction(sState, StopCapture) match {
                                case (_, MoveOk(s2))                        => s2
                                case (_, CaptureRequired(ss2, _, opts2))    => handleCaptureChain(true, ss2, opts2, state, startTime)
                                case _                                      => sState
                            }
                        case _ =>
                            println("Resposta inválida. Tenta novamente.")
                            handleCaptureChain(true, sState, opts, state, startTime)
                    }
            }
        } else {
            if (shouldContinueCaptureChain(state.difficulty)) {
                val choice = opts.head
                println(s"Captura encadeada: ${KonaneLogic.coordToString(choice)}")
                GameEngine.handleAction(sState, ContinueCapture(choice)) match {
                    case (_, MoveOk(s2))                     => s2
                    case (_, CaptureRequired(ss2, _, opts2)) => handleCaptureChain(false, ss2, opts2, state, 0)
                    case _                                   => sState
                }
            } else sState
        }
    }

    @tailrec
    def readAndProcessCaptureChoice(sState: GameState, opts: List[(Int, Int)],
                                    state: GameState, startTime: Long): GameState =
        timedReadNonEmptyTrimmed(startTime, state.timeLimitMillis) match {
            case None =>
                handleTimeout(state); state
            case Some(choiceStr) =>
                scala.util.Try(choiceStr.toInt).toOption match {
                    case Some(idx) if idx >= 1 && idx <= opts.length =>
                        GameEngine.handleAction(sState, ContinueCapture(opts(idx - 1))) match {
                            case (_, MoveOk(s2))                        => s2
                            case (_, CaptureRequired(ss2, _, opts2))    => handleCaptureChain(true, ss2, opts2, state, startTime)
                            case (_, InvalidAction(m))                  => println(m); readAndProcessCaptureChoice(sState, opts, state, startTime)
                            case (_, GameOver(_))                       => sState
                            case _                                      => sState
                        }
                    case _ =>
                        println(s"Entrada inválida. Escolhe um número entre 1 e ${opts.length}.")
                        printCaptureOptions(opts)
                        readAndProcessCaptureChoice(sState, opts, state, startTime)
                }
        }

    def gameLoopState(initial: GameState): Unit = {
        @tailrec
        def loop(state: GameState): Unit = {
            println(KonaneLogic.boardToString(state.board, state.rows, state.cols))
            KonaneLogic.getWinner(state.board, state.currentPlayer, state.rows, state.cols) match {
                case Some(winner) => handleGameWin(winner)
                case None =>
                    if (isHumanTurn(state)) {
                        val startTime = System.currentTimeMillis()
                        print(s"Peca a mover (ou 'undo'/'save'/'random'/'exit'${if (state.mode == "HVC") ", 'restart'" else ""}): ")
                        timedReadNonEmptyTrimmed(startTime, state.timeLimitMillis) match {
                            case None => handleTimeout(state)
                            case Some(fromStr) =>
                                fromStr.trim.toLowerCase match {
                                    case "exit" =>
                                        println("Voltando ao menu...")
                                        menuLoop(state.rows, state.cols, state.timeLimitMillis, state.difficulty)
                                    case "restart" if state.mode == "HVC" =>
                                        println("Recomeçando o jogo...")
                                        startNewGame(state.rows, state.cols, state.playerColorOpt.contains(Stone.Black),
                                            state.rand, state.timeLimitMillis, state.difficulty)
                                    case "save" =>
                                        handleSave(state) match {
                                            case Some(nextState) => loop(nextState)
                                            case None            => menuLoop(state.rows, state.cols, state.timeLimitMillis, state.difficulty)
                                        }
                                    case cmd =>
                                        loop(handleHumanInput(cmd, state, startTime))
                                }
                        }
                    } else {
                        println("Vez do Computador...")
                        GameEngine.handleAction(state, RandomMove) match {
                            case (ns, InvalidAction(msg))            => println(msg); loop(ns)
                            case (_, CaptureRequired(ss, _, opts))   => loop(handleCaptureChain(false, ss, opts, state, 0))
                            case (_, MoveOk(s2))                     => loop(s2)
                            case (ns, _)                             => loop(ns)
                        }
                    }
            }
        }
        loop(initial)
    }

    def handleHumanInput(cmd: String, state: GameState, startTime: Long): GameState =
        cmd match {
            case "undo" =>
                GameEngine.handleAction(state, Undo) match {
                    case (ns, InvalidAction(msg)) => println(msg); ns
                    case (ns, _)                  => ns
                }
            case "random" =>
                GameEngine.handleAction(state, RandomMove) match {
                    case (ns, InvalidAction(msg))          => println(msg); ns
                    case (_, CaptureRequired(ss, _, opts)) => handleCaptureChain(true, ss, opts, state, startTime)
                    case (_, MoveOk(s2))                   => s2
                    case (ns, _)                           => ns
                }
            case other =>
                KonaneLogic.parseInput(other) match {
                    case None => println("Entrada invalida."); state
                    case Some(from) =>
                        print("Para onde: ")
                        timedReadNonEmptyTrimmed(startTime, state.timeLimitMillis) match {
                            case None => handleTimeout(state); state
                            case Some(toStr) =>
                                KonaneLogic.parseInput(toStr) match {
                                    case None     => println("Entrada invalida para destino."); state
                                    case Some(to) =>
                                        GameEngine.handleAction(state, MakeMove(from, to)) match {
                                            case (ns, InvalidAction(msg))          => println(msg); ns
                                            case (_, CaptureRequired(ss, _, opts)) => handleCaptureChain(true, ss, opts, state, startTime)
                                            case (_, MoveOk(s2))                   => s2
                                            case (ns, _)                           => ns
                                        }
                                }
                        }
                }
        }

    def handleSave(state: GameState): Option[GameState] = {
        @tailrec
        def askFileName(): String = {
            print("Nome do ficheiro: ")
            val rawName     = readNonEmptyTrimmed()
            val invalidChars = Set('/', '\\', ':', '*', '?', '"', '<', '>', '|')
            if (rawName.exists(invalidChars.contains(_))) {
                println("Nome invalido. Tenta novamente."); askFileName()
            } else if (FileUtils.fileNameExists(rawName)) {
                println("Ja existe um ficheiro com esse nome. Escolhe outro nome."); askFileName()
            } else rawName
        }

        GameEngine.handleAction(state, Save(askFileName())) match {
            case (_, SaveRequested(b, r, cp, open, rr, cc, mode, pcol, diff, fn, hist)) =>
                if (FileUtils.saveGame(fn, b, r, cp, open, rr, cc, mode, pcol, diff, hist)) {
                    println("Jogo salvo com sucesso! A voltar ao menu..."); None
                } else {
                    println("Falha ao salvar. Tente novamente."); Some(state)
                }
            case (ns, _) => println("Nao foi possivel preparar save."); Some(ns)
        }
    }

    def handleGameWin(winner: Stone): Unit = {
        println("--- FIM DE JOGO ---")
        println("Vencedor: " + (if (winner == Stone.Black) "Pretas (B)" else "Brancas (W)"))
    }
}
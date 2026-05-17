import javafx.animation.{KeyFrame, Timeline}
import javafx.application.Platform
import javafx.fxml.FXML
import javafx.scene.control.*
import javafx.scene.layout.GridPane
import javafx.util.Duration

class BotController extends GameControllerBase {

    @FXML var boardGrid: GridPane = _
    @FXML var timeProgressBar: ProgressBar = _
    @FXML var playerLabel: Label = _
    @FXML var statusLabel: Label = _
    @FXML var randomButton: Button = _
    @FXML var undoButton: Button = _
    @FXML var saveButton: Button = _
    @FXML var restartButton: Button = _
    @FXML var backButton: Button = _

    var currentState: GameState = null
    var boardSize: Int = 8
    var difficulty: String = "facil"
    var tempoLimite: Int = 60
    var selectedPiece: Option[Coord2D] = None
    var highlightedMoves: List[Coord2D] = Nil
    var captureLocked: Boolean = false
    var computerMoveTimer: Timeline = null

    private def stopComputerTimer(): Unit = {
        if (computerMoveTimer != null) {
            computerMoveTimer.stop()
            computerMoveTimer = null
        }
    }

    @FXML
    def initialize(): Unit = {
        randomButton.setOnAction(_ => handleRandomMove())
        undoButton.setOnAction(_ => handleUndo())
        saveButton.setOnAction(_ => handleSave())
        restartButton.setOnAction(_ => handleRestart())
        backButton.setOnAction(_ => backToMenu())
    }

    def setOptions(diff: String, tempo: Int, boardDim: Int, playerColor: Option[Stone] = Some(Stone.Black)): Unit = {
        stopComputerTimer()
        difficulty = diff
        tempoLimite = tempo
        boardSize = boardDim
        val seed = MyRandom(System.currentTimeMillis())
        val (boardReady, openCoords, randAfterSetup) = KonaneLogic.setupBoard(
            KonaneLogic.initBoard(boardSize, boardSize),
            boardSize, boardSize, seed
        )
        val (actualPlayerColor, finalRand) = playerColor match {
            case None =>
                val (randomChoice, seedAfter) = randAfterSetup.nextInt(2)
                (if (randomChoice == 0) Stone.Black else Stone.White, seedAfter)
            case Some(color) => (color, randAfterSetup)
        }
        currentState = GameState(
            boardReady, finalRand, Stone.Black, openCoords,
            boardSize, boardSize, Nil, "HVC", Some(actualPlayerColor),
            tempoLimite * 1000L, difficulty, None
        )
        initializeGameUI()
    }

    def loadGame(board: Board, rand: MyRandom, currentPlayer: Stone, openCoords: List[Coord2D],
                 r: Int, c: Int, playerColorOpt: Option[Stone], savePath: String, diff: String, tempo: Int,
                 history: List[(Board, MyRandom, Stone, List[Coord2D])] = Nil): Unit = {
        stopComputerTimer()
        boardSize = r
        difficulty = diff
        tempoLimite = tempo
        currentState = GameState(
            board, rand, currentPlayer, openCoords, r, c, history,
            "HVC", playerColorOpt,
            tempoLimite * 1000L, difficulty, None
        )
        initializeGameUI()
    }

    override def trySelectPiece(coord: Coord2D): Unit = {
        currentState.board.get(coord) match {
            case Some(piece) if currentState.currentPlayer == piece && currentState.playerColorOpt.contains(piece) =>
                selectedPiece = Some(coord)
                highlightedMoves = KonaneLogic.getValidMovesForPiece(currentState.board, piece, coord, boardSize, boardSize)
                buildBoardUI()
            case _ => ()
        }
    }

    override def makeMove(from: Coord2D, to: Coord2D): Unit = {
        if (timeExpired) {
            new Alert(Alert.AlertType.INFORMATION, "Tempo esgotado! Perdeste. O computador vence.", ButtonType.OK).showAndWait()
            backToMenu()
        } else {
            stopTimer()
            val (ns, res) = GameEngine.handleAction(currentState, MakeMove(from, to))
            handleTurnResult(ns, res)
        }
    }

    override def onGameInitialized(): Unit = {
        val playerColor = currentState.playerColorOpt.getOrElse(Stone.Black)
        if (playerColor == Stone.White) {
            statusLabel.setText("Computador a pensar...")
            scheduleComputerMove(1500)
        }
    }

    override def onTurnTransition(): Unit = {
        if (currentState.playerColorOpt.contains(currentState.currentPlayer)) {
            resetTimer()
            startTimer()
        } else {
            statusLabel.setText("Computador a pensar...")
            scheduleComputerMove(1500)
        }
    }

    override def onCaptureRequired(lastPos: Coord2D, opts: List[Coord2D]): Unit = {
        if (currentState.playerColorOpt.contains(currentState.currentPlayer)) {
            val alert = new Alert(Alert.AlertType.CONFIRMATION)
            alert.setTitle("Captura Encadeada")
            alert.setHeaderText("Capturaste uma peça! Queres continuar a capturar com esta peça?")
            val btnContinue = new ButtonType("Continuar")
            val btnStop     = new ButtonType("Terminar Jogada")
            alert.getButtonTypes.setAll(btnContinue, btnStop)
            val result = alert.showAndWait()

            if (result.isPresent && result.get() == btnContinue) {
                captureLocked = true
                selectedPiece = Some(lastPos)
                highlightedMoves = opts
                buildBoardUI()
                statusLabel.setText("Escolhe a próxima captura no tabuleiro")
            } else {
                captureLocked = false
                clearSelection()
                val (ns2, res2) = GameEngine.handleAction(currentState, StopCapture)
                handleTurnResult(ns2, res2)
            }
        } else {
            val choice = opts.head
            selectedPiece = Some(lastPos)
            highlightedMoves = List(choice)
            buildBoardUI()
            statusLabel.setText("Computador - captura encadeada...")
            val t = new Timeline(new KeyFrame(Duration.millis(1000), _ => {
                val (ns2, res2) = GameEngine.handleAction(currentState, ContinueCapture(choice))
                handleTurnResult(ns2, res2)
            }))
            t.setCycleCount(1)
            computerMoveTimer = t
            t.play()
        }
    }

    override def onGameOverMessage(winner: Stone): String = {
        val humanColor = currentState.playerColorOpt.getOrElse(Stone.Black)
        if (winner == humanColor) "Ganhaste! Parabéns!" else "Perdeste. Boa sorte da próxima vez!"
    }

    override def onTimeout(): Unit = {
        val humanColor = currentState.playerColorOpt.getOrElse(Stone.Black)
        val winner = if (humanColor == Stone.Black) Stone.White else Stone.Black
        stopTimer()
        captureLocked = false
        clearSelection()
        Platform.runLater(() => {
            val alert = new Alert(Alert.AlertType.INFORMATION)
            alert.setTitle("Tempo Esgotado")
            alert.setHeaderText("Perdeste por tempo!")
            alert.setContentText(s"O computador (${winner}) vence.")
            alert.getButtonTypes.setAll(new ButtonType("Voltar ao Menu"))
            alert.showAndWait()
            backToMenu()
        })
    }

    def scheduleComputerMove(delayMs: Int): Unit = {
        val t = new Timeline(new KeyFrame(Duration.millis(delayMs), _ => performComputerMove()))
        t.setCycleCount(1)
        computerMoveTimer = t
        t.play()
    }

    def performComputerMove(): Unit = {
        val (ns, res) = GameEngine.handleAction(currentState, RandomMove)
        handleComputerMoveResult(ns, res)
    }

    def handleComputerMoveResult(ns: GameState, res: TurnResult): Unit = res match {
        case MoveOk(s) =>
            currentState = s
            clearSelection()
            buildBoardUI()
            onTurnTransition()
        case CaptureRequired(s, lastPos, opts) =>
            currentState = s
            onCaptureRequired(lastPos, opts)
        case GameOver(winner) =>
            handleTurnResult(ns, GameOver(winner))
        case _ =>
            val nextPlayer = if (currentState.currentPlayer == Stone.Black) Stone.White else Stone.Black
            currentState = currentState.copy(currentPlayer = nextPlayer)
            clearSelection()
            buildBoardUI()
            resetTimer()
            startTimer()
    }

    def handleUndo(): Unit = {
        if (!captureLocked) {
            stopTimer()
            val (ns, res) = GameEngine.handleAction(currentState, Undo)
            handleTurnResult(ns, res)
        }
    }

    def handleRandomMove(): Unit = {
        if (!captureLocked && currentState.playerColorOpt.contains(currentState.currentPlayer)) {
            if (!timeExpired) {
                stopTimer()
                val (ns, res) = GameEngine.handleAction(currentState, RandomMove)
                handleTurnResult(ns, res)
            } else {
                new Alert(Alert.AlertType.INFORMATION, "Tempo esgotado! Perdeste. O computador vence.", ButtonType.OK).showAndWait()
                backToMenu()
            }
        }
    }

    def handleRestart(): Unit = {
        stopTimer()
        stopComputerTimer()
        captureLocked = false
        clearSelection()
        setOptions(difficulty, tempoLimite, boardSize, currentState.playerColorOpt)
    }
}
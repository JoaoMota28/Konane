import javafx.application.Platform
import javafx.fxml.FXML
import javafx.scene.control.*
import javafx.scene.layout.GridPane

class MultiplayerController extends GameControllerBase {

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
    var tempoLimite: Int = 60
    var selectedPiece: Option[Coord2D] = None
    var highlightedMoves: List[Coord2D] = Nil
    var captureLocked: Boolean = false

    @FXML
    def initialize(): Unit = {
        randomButton.setOnAction(_ => handleRandomMove())
        undoButton.setOnAction(_ => handleUndo())
        saveButton.setOnAction(_ => handleSave())
        restartButton.setOnAction(_ => handleRestart())
        backButton.setOnAction(_ => backToMenu())
    }

    def setOptions(tempo: Int, boardDim: Int): Unit = {
        tempoLimite = tempo
        boardSize = boardDim
        val seed = MyRandom(System.currentTimeMillis())
        val (boardReady, openCoords, randAfterSetup) = KonaneLogic.setupBoard(
            KonaneLogic.initBoard(boardSize, boardSize),
            boardSize, boardSize, seed
        )
        initGameState(boardReady, randAfterSetup, openCoords)
    }

    def loadGame(board: Board, rand: MyRandom, currentPlayer: Stone, openCoords: List[Coord2D],
                 r: Int, c: Int, savePath: String, tempo: Int,
                 history: List[(Board, MyRandom, Stone, List[Coord2D])] = Nil): Unit = {
        boardSize = r
        tempoLimite = tempo
        initGameState(board, rand, openCoords, Some((r, c, currentPlayer)), history)
    }

    def initGameState(board: Board, rand: MyRandom, openCoords: List[Coord2D],
                      stateOpt: Option[(Int, Int, Stone)] = None,
                      history: List[(Board, MyRandom, Stone, List[Coord2D])] = Nil): Unit = {
        val (r, c, currentPlayer) = stateOpt.getOrElse((boardSize, boardSize, Stone.Black))
        currentState = GameState(
            board, rand, currentPlayer, openCoords, r, c, history,
            "HVH", None, tempoLimite * 1000L, "facil", None
        )
        initializeGameUI()
    }

    override def trySelectPiece(coord: Coord2D): Unit = {
        currentState.board.get(coord) match {
            case Some(st) if st == currentState.currentPlayer =>
                selectedPiece = Some(coord)
                highlightedMoves = KonaneLogic.getValidMovesForPiece(currentState.board, st, coord, boardSize, boardSize)
                buildBoardUI()
            case _ => ()
        }
    }

    override def makeMove(from: Coord2D, to: Coord2D): Unit = {
        if (timeExpired) {
            val nextPlayer = if (currentState.currentPlayer == Stone.Black) Stone.White else Stone.Black
            val alert = new Alert(Alert.AlertType.INFORMATION,
                s"Tempo esgotado! ${currentState.currentPlayer} perdeu. Passa para ${nextPlayer}.",
                ButtonType.OK)
            alert.showAndWait()
            currentState = currentState.copy(currentPlayer = nextPlayer)
            captureLocked = false
            clearSelection()
            buildBoardUI()
            resetTimer()
            startTimer()
        } else {
            stopTimer()
            val (ns, res) = GameEngine.handleAction(currentState, MakeMove(from, to))
            handleTurnResult(ns, res)
        }
    }

    override def onCaptureRequired(lastPos: Coord2D, opts: List[Coord2D]): Unit = {
        val alert = new Alert(Alert.AlertType.CONFIRMATION)
        alert.setTitle("Captura Encadeada")
        alert.setHeaderText(s"${currentState.currentPlayer} capturou uma peça! Continuar a capturar?")
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
    }

    override def onGameOverMessage(winner: Stone): String = s"Vencedor: $winner! Parabéns!"

    override def onTimeout(): Unit = {
        val loser      = currentState.currentPlayer
        val nextPlayer = if (loser == Stone.Black) Stone.White else Stone.Black
        stopTimer()
        captureLocked = false
        clearSelection()
        currentState = currentState.copy(currentPlayer = nextPlayer)
        buildBoardUI()
        Platform.runLater(() => {
            val alert = new Alert(Alert.AlertType.INFORMATION,
                s"Tempo esgotado! ${loser} perdeu o turno. Passa para ${nextPlayer}.",
                ButtonType.OK)
            alert.showAndWait()
            resetTimer()
            startTimer()
        })
    }

    def handleUndo(): Unit = {
        if (!captureLocked) {
            stopTimer()
            val (ns, res) = GameEngine.handleAction(currentState, Undo)
            handleTurnResult(ns, res)
        }
    }

    def handleRandomMove(): Unit = {
        if (!captureLocked) {
            if (!timeExpired) {
                stopTimer()
                val (ns, res) = GameEngine.handleAction(currentState, RandomMove)
                handleTurnResult(ns, res)
            } else {
                val nextPlayer = if (currentState.currentPlayer == Stone.Black) Stone.White else Stone.Black
                val alert = new Alert(Alert.AlertType.INFORMATION,
                    s"Tempo esgotado! ${currentState.currentPlayer} perdeu. Passa para ${nextPlayer}.",
                    ButtonType.OK)
                alert.showAndWait()
                currentState = currentState.copy(currentPlayer = nextPlayer)
                captureLocked = false
                clearSelection()
                buildBoardUI()
                resetTimer()
                startTimer()
            }
        }
    }

    def handleRestart(): Unit = {
        stopTimer()
        captureLocked = false
        clearSelection()
        setOptions(tempoLimite, boardSize)
    }
}
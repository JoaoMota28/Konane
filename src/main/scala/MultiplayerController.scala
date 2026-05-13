import javafx.fxml.FXML
import javafx.scene.control.*
import javafx.scene.layout.GridPane

class MultiplayerController extends GameControllerBase {

  @FXML var boardGrid: GridPane = _
  @FXML var timeProgressBar: ProgressBar = _
  @FXML var playerLabel: Label = _
  @FXML var statusLabel: Label = _
  @FXML private var randomButton: Button = _
  @FXML private var undoButton: Button = _
  @FXML private var saveButton: Button = _
  @FXML private var restartButton: Button = _
  @FXML private var backButton: Button = _

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
     val (boardReady, openCoords, randAfterSetup) = GameEngine.setupBoard(
       GameEngine.initBoard(boardSize, boardSize),
       boardSize, boardSize, seed
     )

     currentState = GameState(
       boardReady, randAfterSetup, Stone.Black, openCoords,
       boardSize, boardSize, Nil, "HVH", None, None,
       tempoLimite * 1000L, "facil", None
     )

     initializeGameUI()
   }

    def loadGame(board: Board, rand: MyRandom, currentPlayer: Stone, openCoords: List[Coord2D],
                 r: Int, c: Int, savePath: String, tempo: Int): Unit = {
      boardSize = r
      tempoLimite = tempo

      currentState = GameState(
        board, rand, currentPlayer, openCoords, r, c, Nil,
        "HVH", None, Some(savePath),
        tempoLimite * 1000L, "facil", None
      )

     initializeGameUI()
    }

   protected def handleCellClicked(coord: Coord2D): Unit = {
     if (captureLocked) {
       // Only valid continuation destinations are accepted
       if (highlightedMoves.contains(coord)) {
         val (ns, res) = GameEngine.handleAction(currentState, ContinueCapture(coord))
         handleTurnResult(ns, res)
       }
     } else if (selectedPiece.isDefined) {
       if (highlightedMoves.contains(coord)) {
         val from = selectedPiece.get
         clearSelection()
         makeMove(from, coord)
       } else {
         clearSelection()
         trySelectPiece(coord)
       }
     } else {
       trySelectPiece(coord)
     }
   }

  private def trySelectPiece(coord: Coord2D): Unit = {
    currentState.board.get(coord) match {
      case Some(st) if st == currentState.currentPlayer =>
        selectedPiece = Some(coord)
        highlightedMoves = GameEngine.getValidMovesForPiece(currentState.board, st, coord, boardSize, boardSize)
        buildBoardUI()
      case _ => ()
    }
  }



    private def makeMove(from: Coord2D, to: Coord2D): Unit = {
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

    /**
     * Override onCaptureRequired: in HVH mode, always ask the current player (both are human).
     */
    override protected def onCaptureRequired(lastPos: Coord2D, opts: List[Coord2D]): Unit = {
      val alert = new Alert(Alert.AlertType.CONFIRMATION)
      alert.setTitle("Captura Encadeada")
      alert.setHeaderText(s"${currentState.currentPlayer} capturou uma peça! Continuar a capturar?")
      val btnContinue = new ButtonType("Continuar")
      val btnStop = new ButtonType("Terminar Jogada")
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

    /**
     * Override game over message for HVH context.
     */
    override protected def onGameOverMessage(winner: Stone): String = s"Vencedor: $winner! Parabéns!"

    private def handleUndo(): Unit = {
      if (!captureLocked) {
        stopTimer()
        val (ns, res) = GameEngine.handleAction(currentState, Undo)
        handleTurnResult(ns, res)
      }
    }

    private def handleRandomMove(): Unit = {
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

   private def handleRestart(): Unit = {
     stopTimer()
     captureLocked = false
     clearSelection()
     setOptions(tempoLimite, boardSize)
   }

 }

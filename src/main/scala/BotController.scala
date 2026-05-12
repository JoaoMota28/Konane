import javafx.animation.{KeyFrame, Timeline}
import javafx.fxml.FXML
import javafx.scene.control.*
import javafx.scene.layout.GridPane
import javafx.stage.Stage
import javafx.util.Duration

class BotController extends GameControllerBase {

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
  private var difficulty: String = "facil"
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
    backButton.setOnAction(_ => handleBack())
  }

    def setOptions(diff: String, tempo: Int, boardDim: Int, playerColor: Option[Stone] = Some(Stone.Black)): Unit = {
      difficulty = diff
      tempoLimite = tempo
      boardSize = boardDim

     val seed = MyRandom(System.currentTimeMillis())
     val (boardReady, openCoords, randAfterSetup) = GameEngine.setupBoard(
       GameEngine.initBoard(boardSize, boardSize),
       boardSize, boardSize, seed
     )

     // Determine the player's actual color and get the updated random state
     val (actualPlayerColor, finalRand) = playerColor match {
       case None => // Random - destructure the tuple to get both the choice and the updated seed
         val (randomChoice, seedAfter) = randAfterSetup.nextInt(2)
         (if (randomChoice == 0) Stone.Black else Stone.White, seedAfter)
       case Some(color) => (color, randAfterSetup)
     }

     currentState = GameState(
       boardReady, finalRand, Stone.Black, openCoords,
       boardSize, boardSize, Nil, "HVC", Some(actualPlayerColor), None,
       tempoLimite * 1000L, difficulty, None
     )

     initializeGameUI()
   }

    def loadGame(board: Board, rand: MyRandom, currentPlayer: Stone, openCoords: List[Coord2D],
                 r: Int, c: Int, playerColorOpt: Option[Stone], savePath: String, diff: String, tempo: Int): Unit = {
      boardSize = r
      difficulty = diff
      tempoLimite = tempo

     currentState = GameState(
       board, rand, currentPlayer, openCoords, r, c, Nil,
       "HVC", playerColorOpt, Some(savePath),
       tempoLimite * 1000L, difficulty, None
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
       case Some(piece) if currentState.currentPlayer == piece && currentState.playerColorOpt.contains(piece) =>
         selectedPiece = Some(coord)
         highlightedMoves = GameEngine.getValidMovesForPiece(currentState.board, piece, coord, boardSize, boardSize)
         buildBoardUI()
       case _ => ()
     }
   }



    private def makeMove(from: Coord2D, to: Coord2D): Unit = {
      if (timeExpired) {
        new Alert(Alert.AlertType.INFORMATION, "Tempo esgotado! Perdeste. O computador vence.", ButtonType.OK).showAndWait()
        backToMenu()
      } else {
        stopTimer()
        val (ns, res) = GameEngine.handleAction(currentState, MakeMove(from, to))
        handleTurnResult(ns, res)
      }
     }

    /**
     * Override onGameInitialized: if player is White, computer (Black) goes first.
     */
    override protected def onGameInitialized(): Unit = {
      val playerColor = currentState.playerColorOpt.getOrElse(Stone.Black)
      if (playerColor == Stone.White) {
        statusLabel.setText("Computador a pensar...")
        scheduleComputerMove(1500)
      }
    }

    /**
     * Override onTurnTransition: in HVC mode, check if it's computer's turn.
     */
    override protected def onTurnTransition(): Unit = {
      if (currentState.playerColorOpt.contains(currentState.currentPlayer)) {
        // It's the human player's turn
        resetTimer()
        startTimer()
      } else {
        // It's the computer's turn
        statusLabel.setText("Computador a pensar...")
        scheduleComputerMove(1500)
      }
    }

    /**
     * Override onCaptureRequired: different behavior for human vs computer.
     */
    override protected def onCaptureRequired(lastPos: Coord2D, opts: List[Coord2D]): Unit = {
      if (currentState.playerColorOpt.contains(currentState.currentPlayer)) {
        // Human player: ask continue or stop
        val alert = new Alert(Alert.AlertType.CONFIRMATION)
        alert.setTitle("Captura Encadeada")
        alert.setHeaderText("Capturaste uma peça! Queres continuar a capturar com esta peça?")
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
      } else {
        // Bot: show highlight then apply one capture after a delay
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
        t.play()
      }
    }

    /**
     * Override game over message for HVC context.
     */
    override protected def onGameOverMessage(winner: Stone): String = {
      val humanColor = currentState.playerColorOpt.getOrElse(Stone.Black)
      if (winner == humanColor) "Ganhaste! Parabéns!" else "Perdeste. Boa sorte da próxima vez!"
    }

  private def scheduleComputerMove(delayMs: Int): Unit = {
    val t = new Timeline(new KeyFrame(Duration.millis(delayMs), _ => performComputerMove()))
    t.setCycleCount(1)
    t.play()
  }

   private def performComputerMove(): Unit = {
     val (ns, res) = GameEngine.handleAction(currentState, RandomMove)
     res match {
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
         // Bot has no moves — shouldn't happen normally
         val nextPlayer = if (currentState.currentPlayer == Stone.Black) Stone.White else Stone.Black
         currentState = currentState.copy(currentPlayer = nextPlayer)
         clearSelection()
         buildBoardUI()
         resetTimer()
         startTimer()
     }
   }

   private def handleUndo(): Unit = {
     if (!captureLocked) {
       stopTimer()
       val (ns, res) = GameEngine.handleAction(currentState, Undo)
       handleTurnResult(ns, res)
     }
   }

   private def handleRandomMove(): Unit = {
     if (!captureLocked && (currentState.playerColorOpt.nonEmpty && currentState.playerColorOpt.contains(currentState.currentPlayer))) {
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

  private def handleSave(): Unit = {
    stopTimer()
    val (ns, res) = GameEngine.handleAction(currentState, Save)
    handleTurnResult(ns, res)
  }

  private def handleRestart(): Unit = {
    stopTimer()
    captureLocked = false
    clearSelection()
    val playerColor = currentState.playerColorOpt
    setOptions(difficulty, tempoLimite, boardSize, playerColor)
  }

   private def handleBack(): Unit = {
     stopTimer()
     boardGrid.getScene.getWindow.asInstanceOf[Stage].close()
   }

  }

import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Label, ProgressBar, Alert, ButtonType}
import javafx.scene.layout.GridPane
import javafx.scene.image.{Image, ImageView}
import javafx.animation.{KeyFrame, Timeline, Animation}
import javafx.util.Duration
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import KonaneLogic.*

class BotController {

  @FXML private var boardGrid: GridPane = _
  @FXML private var timeProgressBar: ProgressBar = _
  @FXML private var playerLabel: Label = _
  @FXML private var statusLabel: Label = _
  @FXML private var randomButton: Button = _
  @FXML private var undoButton: Button = _
  @FXML private var saveButton: Button = _
  @FXML private var restartButton: Button = _
  @FXML private var backButton: Button = _

  private val blackImg = loadImage("src/Images/Black.png")
  private val whiteImg = loadImage("src/Images/White.png")

  private var currentState: GameState = null
  private var boardSize: Int = 8
  private var difficulty: String = "facil"
  private var tempoLimite: Int = 60
  private var tempoRestante: Int = 60
  private var timer: Timeline = null
  private var timeExpired: Boolean = false

  // Board interaction state
  private var selectedPiece: Option[Coord2D] = None
  private var highlightedMoves: List[Coord2D] = Nil
  // When true, the player is in a chain-capture: only clicks on highlightedMoves are accepted
  private var captureLocked: Boolean = false

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
    tempoRestante = tempo
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
      boardSize, boardSize, Nil, true, false, "HVC", Some(actualPlayerColor), None,
      tempoLimite * 1000L, difficulty, None
    )

    captureLocked = false
    buildBoardUI()

    // If player is White, computer (Black) goes first
    if (actualPlayerColor == Stone.White) {
      statusLabel.setText("Computador a pensar...")
      scheduleComputerMove(1500)
    } else {
      resetTimer()
      startTimer()
    }
  }

  def loadGame(board: Board, rand: MyRandom, currentPlayer: Stone, openCoords: List[Coord2D],
               r: Int, c: Int, playerColorOpt: Option[Stone], savePath: String, diff: String, tempo: Int): Unit = {
    boardSize = r
    difficulty = diff
    tempoLimite = tempo
    tempoRestante = tempo

    currentState = GameState(
      board, rand, currentPlayer, openCoords, r, c, Nil,
      true, false, "HVC", playerColorOpt, Some(savePath),
      tempoLimite * 1000L, difficulty, None
    )

    captureLocked = false
    buildBoardUI()
    resetTimer()
    startTimer()
  }

  private def buildBoardUI(): Unit = {
    boardGrid.getChildren.clear()
    // ...existing code...
    drawRows(0)
    updateStatus()
  }

  @tailrec
  private def drawRows(i: Int): Unit = {
    if (i < boardSize) {
      drawColumns(i, 0)
      drawRows(i + 1)
    }
  }

  @tailrec
  private def drawColumns(i: Int, j: Int): Unit = {
    if (j < boardSize) {
      createCell(i, j)
      drawColumns(i, j + 1)
    }
  }

   private def createCell(i: Int, j: Int): Unit = {
     val btn = new Button()
     btn.setMinSize(60, 60)
     btn.setMaxSize(60, 60)

     val coord = (i, j)

     // Base color: checkerboard pattern
     var baseStyle = if ((i + j) % 2 == 0) "-fx-base: beige;" else "-fx-base: sandybrown;"

     // Add piece graphic
     currentState.board.get(coord) match {
       case Some(Stone.Black) =>
         val iv = new ImageView(blackImg); iv.setFitWidth(48); iv.setFitHeight(48)
         btn.setGraphic(iv)
       case Some(Stone.White) =>
         val iv = new ImageView(whiteImg); iv.setFitWidth(48); iv.setFitHeight(48)
         btn.setGraphic(iv)
       case None =>
         btn.setGraphic(null)
     }

     // Apply highlighting or selection border
     if (selectedPiece.contains(coord)) {
       baseStyle += "-fx-border-color: red; -fx-border-width: 3;"
     } else if (highlightedMoves.contains(coord)) {
       baseStyle += "-fx-base: gold;"
     }

     btn.setStyle(baseStyle)
     btn.setOnAction(_ => handleCellClicked(coord))
     boardGrid.add(btn, j, i)
   }

  private def handleCellClicked(coord: Coord2D): Unit = {
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

  private def clearSelection(): Unit = {
    selectedPiece = None
    highlightedMoves = Nil
  }

   private def makeMove(from: Coord2D, to: Coord2D): Unit = {
     if (timeExpired) {
       new Alert(Alert.AlertType.INFORMATION, "Tempo esgotado! Perdeste. O computador vence.", ButtonType.OK).showAndWait()
       backToMenu()
       return
     }
     stopTimer()
     val (ns, res) = GameEngine.handleAction(currentState, MakeMove(from, to))
     handleTurnResult(ns, res)
   }

   private def handleTurnResult(ns: GameState, res: TurnResult): Unit = {
     res match {

       case MoveOk(s) =>
         captureLocked = false
         clearSelection()
         currentState = s
         buildBoardUI()
         // Check if it's the human player's turn or the computer's turn
         if (currentState.playerColorOpt.contains(currentState.currentPlayer)) {
           // It's the human player's turn
           resetTimer()
           startTimer()
         } else {
           // It's the computer's turn
           statusLabel.setText("Computador a pensar...")
           scheduleComputerMove(1500)
         }

       case CaptureRequired(s, lastPos, opts) =>
         currentState = s
         buildBoardUI()

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
             // Lock piece and highlight valid destinations on board
             captureLocked = true
             selectedPiece = Some(lastPos)
             highlightedMoves = opts
             buildBoardUI()
             statusLabel.setText("Escolhe a próxima captura no tabuleiro")
           } else {
             // Player stops — end turn
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

       case InvalidAction(msg) =>
         captureLocked = false
         new Alert(Alert.AlertType.INFORMATION, msg, ButtonType.OK).showAndWait()
         resetTimer()
         startTimer()

       case GameOver(winner) =>
         stopTimer()
         captureLocked = false
         clearSelection()
         currentState = ns  // Update state with the winning move
         buildBoardUI()     // Display the final board state
         val humanColor = currentState.playerColorOpt.getOrElse(Stone.Black)
         val msg = if (winner == humanColor) "Ganhaste! Parabéns!" else "Perdeste. Boa sorte da próxima vez!"
         val alert = new Alert(Alert.AlertType.INFORMATION)
         alert.setTitle("Fim de Jogo")
         alert.setHeaderText(msg)
         alert.setContentText(s"Vencedor: $winner")
         alert.getButtonTypes.setAll(new ButtonType("Voltar ao Menu"))
         alert.showAndWait()
         backToMenu()

       case SaveRequested(b, r, p, open, rr, cc, md, pcol, diff) =>
         val ok = FileUtils.saveGame(b, r, p, open, rr, cc, md, pcol, diff)
         val alert = if (!ok)
           new Alert(Alert.AlertType.ERROR, "Falha ao salvar.", ButtonType.OK)
         else
           new Alert(Alert.AlertType.INFORMATION, "Jogo salvo com sucesso!", ButtonType.OK)
         alert.showAndWait()
         if (ok) backToMenu()

       case _ => ()
     }
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
        // Check if it's the human player's turn or the computer's turn
        if (currentState.playerColorOpt.contains(currentState.currentPlayer)) {
          // It's the human player's turn
          resetTimer()
          startTimer()
        } else {
          // It's the computer's turn
          scheduleComputerMove(1500)
        }

      case CaptureRequired(s, lastPos, opts) =>
        // Show highlight then apply one capture at a time with delay
        currentState = s
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
    if (captureLocked) return
    stopTimer()
    val (ns, res) = GameEngine.handleAction(currentState, Undo)
    handleTurnResult(ns, res)
  }

  private def handleRandomMove(): Unit = {
    if (captureLocked) return
    if (currentState.playerColorOpt.isEmpty || !currentState.playerColorOpt.contains(currentState.currentPlayer)) {
      return
    }
    if (timeExpired) {
      new Alert(Alert.AlertType.INFORMATION, "Tempo esgotado! Perdeste. O computador vence.", ButtonType.OK).showAndWait()
      backToMenu()
      return
    }
    stopTimer()
    val (ns, res) = GameEngine.handleAction(currentState, RandomMove)
    handleTurnResult(ns, res)
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

  private def backToMenu(): Unit = {
    boardGrid.getScene.getWindow.asInstanceOf[Stage].close()
  }

  private def resetTimer(): Unit = {
     timeProgressBar.setProgress(1.0)
     tempoRestante = tempoLimite
     timeExpired = false
     if (timer != null) timer.stop()
   }

  private def startTimer(): Unit = {
     val interval = 100
     val steps = (tempoLimite * 10).toInt.max(1)
     var count = 0

     timer = new Timeline(new KeyFrame(Duration.millis(interval), _ => {
       count += 1
       val p = 1.0 - count.toDouble / steps
       timeProgressBar.setProgress(Math.max(0.0, p))

       if (p <= 0.0) {
         timer.stop()
         timeExpired = true
         statusLabel.setText("Tempo esgotado! Perdeste!")
       }
     }))
     timer.setCycleCount(Animation.INDEFINITE)
     timer.play()
   }

  private def stopTimer(): Unit = { if (timer != null) timer.stop() }

  private def updateStatus(): Unit = {
    playerLabel.setText(s"Turno: ${currentState.currentPlayer}")
    statusLabel.setText(if (captureLocked) "Escolhe a próxima captura no tabuleiro" else "Pronto")
  }

  private def loadImage(path: String): Image = {
    val f = new java.io.File(path)
    if (f.exists()) new Image(f.toURI.toString) else new Image(getClass.getResourceAsStream("/Black.png"))
  }
}
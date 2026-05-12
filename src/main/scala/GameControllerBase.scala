import javafx.animation.{Animation, KeyFrame, Timeline}
import javafx.application.Platform
import javafx.scene.control.*
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.GridPane
import javafx.stage.Stage
import javafx.util.Duration

import scala.annotation.tailrec

/**
 * Base trait for game controllers (Bot and Multiplayer).
 * Contains shared board UI, timer, and image loading logic.
 */
trait GameControllerBase {

  // Abstract fields that subclasses must provide
  def boardGrid: GridPane
  def timeProgressBar: ProgressBar
  def playerLabel: Label
  def statusLabel: Label

  // Mutable state getters/setters - subclasses must implement
  var currentState: GameState
  var boardSize: Int
  var tempoLimite: Int
  var selectedPiece: Option[Coord2D]
  var highlightedMoves: List[Coord2D]
  var captureLocked: Boolean

  // Lazy-loaded images
  lazy val blackImg: Image = loadImage("src/Images/Black.png")
  lazy val whiteImg: Image = loadImage("src/Images/White.png")

  protected var timer: Timeline = null
  protected var timeExpired: Boolean = false

  /**
   * Build the entire game board UI by clearing and redrawing all cells.
   */
  def buildBoardUI(): Unit = {
    boardGrid.getChildren.clear()
    drawRows(0)
    updateStatus()
  }

  /**
   * Recursively draw all rows of the board.
   */
  @tailrec
  private def drawRows(i: Int): Unit = {
    if (i < boardSize) {
      drawColumns(i, 0)
      drawRows(i + 1)
    }
  }

  /**
   * Recursively draw all columns for a given row.
   */
  @tailrec
  private def drawColumns(i: Int, j: Int): Unit = {
    if (j < boardSize) {
      createCell(i, j)
      drawColumns(i, j + 1)
    }
  }

  /**
   * Create a single cell button for the board.
   */
  protected def createCell(i: Int, j: Int): Unit = {
    val btn = new Button()
    btn.setMinSize(60, 60)
    btn.setMaxSize(60, 60)
    btn.setFocusTraversable(false)

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

  /**
   * Handle cell clicks - to be implemented by subclasses.
   */
  protected def handleCellClicked(coord: Coord2D): Unit

  /**
   * Clear current selection and highlighted moves.
   */
  protected def clearSelection(): Unit = {
    selectedPiece = None
    highlightedMoves = Nil
  }

  /**
   * Reset timer to full duration.
   */
  protected def resetTimer(): Unit = {
    timeProgressBar.setProgress(1.0)
    timeExpired = false
    if (timer != null) timer.stop()
  }

  /**
   * Start the countdown timer.
   */
  protected def startTimer(): Unit = {
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
        statusLabel.setText("Tempo esgotado!")
      }
    }))
    timer.setCycleCount(Animation.INDEFINITE)
    timer.play()
  }

  /**
   * Stop the timer.
   */
  protected def stopTimer(): Unit = {
    if (timer != null) timer.stop()
  }

  /**
   * Update status labels.
   */
  protected def updateStatus(): Unit = {
    playerLabel.setText(s"Turno: ${currentState.currentPlayer}")
    statusLabel.setText(if (captureLocked) "Escolhe a próxima captura no tabuleiro" else "Pronto")
  }

  /**
   * Load an image from file, with fallback to resource.
   */
  protected def loadImage(path: String): Image = {
    val f = new java.io.File(path)
    if (f.exists()) new Image(f.toURI.toString) else new Image(getClass.getResourceAsStream("/Black.png"))
  }

  /**
   * Close the game window and return to menu.
   */
  protected def backToMenu(): Unit = {
    boardGrid.getScene.getWindow.asInstanceOf[Stage].close()
  }

  /**
   * Initialize game state and UI after setup or load.
   * Called by both setOptions and loadGame in subclasses.
   */
  protected def initializeGameUI(): Unit = {
    captureLocked = false
    buildBoardUI()
    resetTimer()
    startTimer()
    onGameInitialized() // Hook for subclass-specific behavior
  }

  /**
   * Hook called after game initialization. Override in subclasses for specific behavior.
   * In BotController: check if computer should move first.
   * In MultiplayerController: no special action needed.
   */
  protected def onGameInitialized(): Unit = ()

  /**
   * Handle turn results - unified logic for both controllers with hooks for differences.
   */
  protected def handleTurnResult(ns: GameState, res: TurnResult): Unit = {
    res match {
      case MoveOk(s) =>
        captureLocked = false
        clearSelection()
        currentState = s
        buildBoardUI()
        onTurnTransition() // Hook: in HVC, schedule computer; in HVH, just reset timer

      case CaptureRequired(s, lastPos, opts) =>
        currentState = s
        buildBoardUI()
        onCaptureRequired(lastPos, opts) // Hook: different for human vs computer

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
        Platform.runLater(() => {
          val alert = new Alert(Alert.AlertType.INFORMATION)
          alert.setTitle("Fim de Jogo")
          alert.setHeaderText(onGameOverMessage(winner))
          alert.setContentText(s"Vencedor: $winner")
          alert.getButtonTypes.setAll(new ButtonType("Voltar ao Menu"))
          alert.showAndWait()
          backToMenu()
        })

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

  /**
   * Hook: called when it's time to transition turns. Override for HVC to schedule computer move.
   */
  protected def onTurnTransition(): Unit = {
    resetTimer()
    startTimer()
  }

  /**
   * Hook: called when a capture is required. Override for specific UI behavior.
   */
  protected def onCaptureRequired(lastPos: Coord2D, opts: List[Coord2D]): Unit = {
    // Default: show alert and lock capture
    val alert = new Alert(Alert.AlertType.CONFIRMATION)
    alert.setTitle("Captura Encadeada")
    alert.setHeaderText(captureRequiredMessage())
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
   * Hook: customize GameOver message. Override in subclasses.
   */
  protected def onGameOverMessage(winner: Stone): String = s"Vencedor: $winner! Parabéns!"

  /**
   * Hook: customize capture required message. Override in subclasses.
   */
  protected def captureRequiredMessage(): String = "Capturaste uma peça! Queres continuar a capturar?"
}





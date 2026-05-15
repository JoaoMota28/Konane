import javafx.animation.{Animation, KeyFrame, Timeline}
import javafx.application.Platform
import javafx.fxml.FXMLLoader
import javafx.scene.control.*
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.GridPane
import javafx.scene.Parent
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
  protected var primaryStage: Stage = _

  def setPrimaryStage(s: Stage): Unit = {
    primaryStage = s
  }

  /**
   * Build the entire game board UI by clearing and redrawing all cells.
   */
  def buildBoardUI(): Unit = {
    boardGrid.getChildren.clear()
    drawBoard(0, 0)
    updateStatus()
  }

  /**
   * Recursively draw the entire board (rows first, then columns).
   */
  @tailrec
  private def drawBoard(i: Int, j: Int): Unit = {
    if (i < boardSize) {
      if (j < boardSize) {
        createCell(i, j)
        drawBoard(i, j + 1)
      } else {
        drawBoard(i + 1, 0)
      }
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

    timer = new Timeline(new KeyFrame(Duration.millis(interval), { evt =>
      val currentValue = timeProgressBar.getProgress
      if (currentValue <= 0.0) {
        timer.stop()
        timeExpired = true
        onTimeout()
      } else {
        timeProgressBar.setProgress(Math.max(0.0, currentValue - 1.0 / steps))
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
   * Called by the timer when time expires — override in subclasses for
   * game-specific timeout behaviour (forfeit current player, show alert, etc.).
   * Default: just updates the status label.
   */
  protected def onTimeout(): Unit = {
    statusLabel.setText("Tempo esgotado!")
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

  // Small UI helpers to reduce duplicated Alert construction
  protected def showInfo(title: String, message: String): Unit = {
    val alert = new Alert(Alert.AlertType.INFORMATION)
    alert.setTitle(title)
    alert.setHeaderText(null)
    alert.setContentText(message)
    alert.getButtonTypes.setAll(ButtonType.OK)
    alert.showAndWait()
  }

  protected def showError(title: String, message: String): Unit = {
    val alert = new Alert(Alert.AlertType.ERROR)
    alert.setTitle(title)
    alert.setHeaderText(null)
    alert.setContentText(message)
    alert.getButtonTypes.setAll(ButtonType.OK)
    alert.showAndWait()
  }

  protected def askChoice(title: String, header: String, opt1: String, opt2: String): Boolean = {
    val alert = new Alert(Alert.AlertType.CONFIRMATION)
    val btn1 = new ButtonType(opt1)
    val btn2 = new ButtonType(opt2)
    alert.getButtonTypes.setAll(btn1, btn2)
    alert.setTitle(title)
    alert.setHeaderText(header)
    val res = alert.showAndWait()
    res.isPresent && res.get() == btn1
  }

  // Callback set by MainController so returning to menu reuses the
  // original MainController instance (preserving options state).
  protected var backToMenuFn: () => Unit = () => {
    val loader = new FXMLLoader(getClass.getResource("/MainView.fxml"))
    val root = loader.load[Parent]()
    val ctrl = loader.getController[MainController]
    ctrl.setPrimaryStage(primaryStage)
    primaryStage.getScene.setRoot(root)
  }

  def setBackToMenuFn(f: () => Unit): Unit = backToMenuFn = f

  /**
   * Close the game window and return to menu.
   * Uses the injected callback when available (preserves MainController state).
   */
  protected def backToMenu(): Unit = backToMenuFn()

  /**
   * Handle save action - shows dialog for filename selection.
   */
  protected def handleSave(): Unit = {
    stopTimer()
    val dialog = new javafx.scene.control.TextInputDialog()
    dialog.setTitle("Guardar Jogo")
    dialog.setHeaderText("Escolhe um nome para o ficheiro de gravação:")
    dialog.setContentText("Nome:")
    val result = dialog.showAndWait()
    if (!result.isPresent || result.get().trim.isEmpty) {
      resetTimer()
      startTimer()
    } else {
      val name = result.get().trim
      if (FileUtils.fileNameExists(name)) {
        new Alert(Alert.AlertType.ERROR, "Já existe um ficheiro com esse nome. Escolhe outro nome.", ButtonType.OK).showAndWait()
        resetTimer()
        startTimer()
      } else {
        val (ns, res) = GameEngine.handleAction(currentState, Save(name))
        handleTurnResult(ns, res)
      }
    }
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
        showInfo("Aviso", msg)
        resetTimer()
        startTimer()

      case GameOver(winner) =>
        stopTimer()
        captureLocked = false
        clearSelection()
        currentState = ns  // Update state with the winning move
        buildBoardUI()     // Display the final board state
        Platform.runLater(() => {
          showInfo("Fim de Jogo", s"${onGameOverMessage(winner)}\nVencedor: $winner")
          backToMenu()
        })

      case SaveRequested(b, r, p, open, rr, cc, md, pcol, diff, fileName, hist) =>
        val ok = FileUtils.saveGame(fileName, b, r, p, open, rr, cc, md, pcol, diff, hist)
        if (!ok) showError("Erro", "Falha ao salvar.") else showInfo("Salvar", "Jogo salvo com sucesso!")
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
    // Default: ask user and act accordingly using helper
    val cont = askChoice("Captura Encadeada", captureRequiredMessage(), "Continuar", "Terminar Jogada")
    if (cont) {
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
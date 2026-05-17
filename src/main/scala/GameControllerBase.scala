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

trait GameControllerBase {

    def boardGrid: GridPane
    def timeProgressBar: ProgressBar
    def playerLabel: Label
    def statusLabel: Label

    var currentState: GameState
    var boardSize: Int
    var tempoLimite: Int
    var selectedPiece: Option[Coord2D]
    var highlightedMoves: List[Coord2D]
    var captureLocked: Boolean

    lazy val blackImg: Image = loadImage("src/Images/Black.png")
    lazy val whiteImg: Image = loadImage("src/Images/White.png")

    var timer: Timeline = null
    var timeExpired: Boolean = false
    var primaryStage: Stage = _

    def setPrimaryStage(s: Stage): Unit = {
        primaryStage = s
    }

    def buildBoardUI(): Unit = {
        boardGrid.getChildren.clear()
        drawBoard(0, 0)
        updateStatus()
    }

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

    def createCell(i: Int, j: Int): Unit = {
        val btn = new Button()
        btn.setMinSize(60, 60)
        btn.setMaxSize(60, 60)
        btn.setFocusTraversable(false)

        val coord = (i, j)

        var baseStyle = if ((i + j) % 2 == 0) "-fx-base: beige;" else "-fx-base: sandybrown;"

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

        if (selectedPiece.contains(coord)) {
            baseStyle += "-fx-border-color: red; -fx-border-width: 3;"
        } else if (highlightedMoves.contains(coord)) {
            baseStyle += "-fx-base: gold;"
        }

        btn.setStyle(baseStyle)
        btn.setOnAction(_ => handleCellClicked(coord))
        boardGrid.add(btn, j, i)
    }

    def handleCellClicked(coord: Coord2D): Unit = {
        if (captureLocked) {
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

    def trySelectPiece(coord: Coord2D): Unit
    def makeMove(from: Coord2D, to: Coord2D): Unit
    def clearSelection(): Unit = {
        selectedPiece = None
        highlightedMoves = Nil
    }

    def resetTimer(): Unit = {
        if (timer != null) timer.stop()
        timeProgressBar.setProgress(1.0)
        timeExpired = false
    }

    def startTimer(): Unit = {
        if (timer != null) timer.stop()

        val interval = 100
        val totalSteps = (tempoLimite * 10).toInt.max(1)
        var stepsRemaining = totalSteps

        timer = new Timeline(new KeyFrame(Duration.millis(interval), { _ =>
            stepsRemaining -= 1
            if (stepsRemaining <= 0) {
                timer.stop()
                timeProgressBar.setProgress(0.0)
                timeExpired = true
                onTimeout()
            } else {
                timeProgressBar.setProgress(stepsRemaining.toDouble / totalSteps)
            }
        }))
        timer.setCycleCount(Animation.INDEFINITE)
        timer.play()
    }

    def stopTimer(): Unit = {
        if (timer != null) timer.stop()
    }

    def onTimeout(): Unit = {
        statusLabel.setText("Tempo esgotado!")
    }

    def updateStatus(): Unit = {
        playerLabel.setText(s"Turno: ${currentState.currentPlayer}")
        statusLabel.setText(if (captureLocked) "Escolhe a próxima captura no tabuleiro" else "Pronto")
    }

    def loadImage(path: String): Image = {
        val f = new java.io.File(path)
        if (f.exists()) new Image(f.toURI.toString) else new Image(getClass.getResourceAsStream("/Black.png"))
    }

    def showInfo(title: String, message: String): Unit = {
        val alert = new Alert(Alert.AlertType.INFORMATION)
        alert.setTitle(title)
        alert.setHeaderText(null)
        alert.setContentText(message)
        alert.getButtonTypes.setAll(ButtonType.OK)
        alert.showAndWait()
    }

    def showError(title: String, message: String): Unit = {
        val alert = new Alert(Alert.AlertType.ERROR)
        alert.setTitle(title)
        alert.setHeaderText(null)
        alert.setContentText(message)
        alert.getButtonTypes.setAll(ButtonType.OK)
        alert.showAndWait()
    }

    def askChoice(title: String, header: String, opt1: String, opt2: String): Boolean = {
        val alert = new Alert(Alert.AlertType.CONFIRMATION)
        val btn1 = new ButtonType(opt1)
        val btn2 = new ButtonType(opt2)
        alert.getButtonTypes.setAll(btn1, btn2)
        alert.setTitle(title)
        alert.setHeaderText(header)
        val res = alert.showAndWait()
        res.isPresent && res.get() == btn1
    }

    var backToMenuFn: () => Unit = () => {
        val loader = new FXMLLoader(getClass.getResource("/MainView.fxml"))
        val root = loader.load[Parent]()
        val ctrl = loader.getController[MainController]
        ctrl.setPrimaryStage(primaryStage)
        primaryStage.getScene.setRoot(root)
    }

    def setBackToMenuFn(f: () => Unit): Unit = backToMenuFn = f

    def backToMenu(): Unit = backToMenuFn()

    def handleSave(): Unit = {
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

    def initializeGameUI(): Unit = {
        captureLocked = false
        buildBoardUI()
        resetTimer()
        startTimer()
        onGameInitialized()
    }

    def onGameInitialized(): Unit = ()

    def handleTurnResult(ns: GameState, res: TurnResult): Unit = {
        res match {
            case MoveOk(s) =>
                captureLocked = false
                clearSelection()
                currentState = s
                buildBoardUI()
                onTurnTransition()

            case CaptureRequired(s, lastPos, opts) =>
                currentState = s
                buildBoardUI()
                onCaptureRequired(lastPos, opts)

            case InvalidAction(msg) =>
                captureLocked = false
                showInfo("Aviso", msg)
                resetTimer()
                startTimer()

            case GameOver(winner) =>
                stopTimer()
                captureLocked = false
                clearSelection()
                currentState = ns
                buildBoardUI()
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

    def onTurnTransition(): Unit = {
        resetTimer()
        startTimer()
    }

    def onCaptureRequired(lastPos: Coord2D, opts: List[Coord2D]): Unit = {
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

    def onGameOverMessage(winner: Stone): String = s"Vencedor: $winner! Parabéns!"

    def captureRequiredMessage(): String = "Capturaste uma peça! Queres continuar a capturar?"
}
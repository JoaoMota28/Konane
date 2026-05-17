import javafx.application.Platform
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.{Button, Alert, ButtonType, ChoiceDialog}
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import scala.jdk.CollectionConverters.*

class MainController {

    @FXML var botButton: Button = _
    @FXML var multiButton: Button = _
    @FXML var optionsButton: Button = _
    @FXML var loadButton: Button = _
    @FXML var exitButton: Button = _

    var selectedDifficulty: String = "facil"
    var selectedBoardSize: Int = 8
    var selectedTempo: Int = 60
    var primaryStage: Stage = _
    var mainMenuRoot: Parent = _

    def setPrimaryStage(s: Stage): Unit = {
        primaryStage = s
    }

    def switchTo(fxmlPath: String): (Parent, Any) = {
        val loader = new FXMLLoader(getClass.getResource(fxmlPath))
        val root = loader.load[Parent]()
        (root, loader.getController[Any]())
    }

    def showScene(root: Parent): Unit = {
        primaryStage.getScene.setRoot(root)
    }

    def showMainMenu(): Unit = {
        showScene(mainMenuRoot)
    }

    def startBotGame(colorOpt: Option[Stone]): Unit = {
        val (botRoot, botController) = switchTo("/BotView.fxml")
        val ctrl = botController.asInstanceOf[BotController]
        ctrl.setOptions(selectedDifficulty, selectedTempo, selectedBoardSize, colorOpt)
        ctrl.setPrimaryStage(primaryStage)
        ctrl.setBackToMenuFn(() => showMainMenu())
        showScene(botRoot)
    }

    @FXML
    def initialize(): Unit = {
        botButton.setOnAction(_ => handlePlayWithBot())
        multiButton.setOnAction(_ => handleMultiplayer())
        loadButton.setOnAction(_ => handleLoadGame())
        optionsButton.setOnAction(_ => handleOptions())
        exitButton.setOnAction(_ => Platform.exit())
        Platform.runLater(() => mainMenuRoot = botButton.getScene.getRoot)
    }

    @FXML
    def handleOptions(): Unit = {
        val (optionsRoot, optionsController) = switchTo("/OptionsView.fxml")
        val ctrl = optionsController.asInstanceOf[OptionsController]
        ctrl.setInitialOptions(selectedDifficulty, selectedBoardSize, selectedTempo)
        ctrl.setOnClose(() => {
            val (diff, size, tempo) = ctrl.getSelectedOptions
            selectedDifficulty = diff
            selectedBoardSize = size
            selectedTempo = tempo
            showMainMenu()
        })
        showScene(optionsRoot)
    }

    @FXML
    def handlePlayWithBot(): Unit = {
        val (colorRoot, colorController) = switchTo("/ColorSelectionView.fxml")
        val ctrl = colorController.asInstanceOf[ColorSelectionController]
        ctrl.setOnColorSelected((colorOpt: Option[Stone]) => {
            startBotGame(colorOpt)
        })
        showScene(colorRoot)
    }

    @FXML
    def handleMultiplayer(): Unit = {
        val (multiRoot, multiController) = switchTo("/MultiplayerView.fxml")
        val ctrl = multiController.asInstanceOf[MultiplayerController]
        ctrl.setOptions(selectedTempo, selectedBoardSize)
        ctrl.setPrimaryStage(primaryStage)
        ctrl.setBackToMenuFn(() => showMainMenu())
        showScene(multiRoot)
    }

    @FXML
    def handleLoadGame(): Unit = {
        val dir = new java.io.File("saves")
        if (!dir.exists() || dir.listFiles().isEmpty) {
            showAlert("Nenhum ficheiro de salvamento disponível.", Alert.AlertType.INFORMATION)
        } else {
            val files = dir.listFiles().filter(_.getName.endsWith(".txt")).toList
            if (files.isEmpty) {
                showAlert("Nenhum ficheiro de salvamento disponível.", Alert.AlertType.INFORMATION)
            } else {
                val choices = files.map(_.getName)
                val choice = new ChoiceDialog(choices.head, choices.asJava)
                choice.setHeaderText("Escolha ficheiro para carregar")
                val res = choice.showAndWait()
                res.ifPresent(name => loadSelectedGame(files.find(_.getName == name).get))
            }
        }
    }

    def loadSelectedGame(file: java.io.File): Unit = {
        FileUtils.loadGameFromFile(file.getPath) match {
            case Some((board, rand, currentPlayer, openCoords, r, c, md, playerColorOpt, savedDifficulty, savedHistory)) =>
                selectedBoardSize = r
                selectedDifficulty = savedDifficulty
                launchGameWithMode(md, board, rand, currentPlayer, openCoords, r, c, playerColorOpt, savedDifficulty, file.getPath, savedHistory)
            case None =>
                showAlert("Erro ao carregar ficheiro.", Alert.AlertType.ERROR)
        }
    }

    def launchGameWithMode(md: String, board: Board, rand: MyRandom, currentPlayer: Stone,
                           openCoords: List[Coord2D], r: Int, c: Int, playerColorOpt: Option[Stone],
                           difficulty: String, savePath: String,
                           history: List[(Board, MyRandom, Stone, List[Coord2D])] = Nil): Unit = {
        val (gameRoot, gameController) = md match {
            case "HVC" => switchTo("/BotView.fxml")
            case "HVH" => switchTo("/MultiplayerView.fxml")
            case _ => switchTo("/BotView.fxml")
        }

        md match {
            case "HVC" =>
                val bc = gameController.asInstanceOf[BotController]
                bc.loadGame(board, rand, currentPlayer, openCoords, r, c, playerColorOpt, savePath, difficulty, selectedTempo, history)
                bc.setPrimaryStage(primaryStage)
                bc.setBackToMenuFn(() => showMainMenu())
            case "HVH" =>
                val mc = gameController.asInstanceOf[MultiplayerController]
                mc.loadGame(board, rand, currentPlayer, openCoords, r, c, savePath, selectedTempo, history)
                mc.setPrimaryStage(primaryStage)
                mc.setBackToMenuFn(() => showMainMenu())
            case _ => ()
        }
        showScene(gameRoot)
    }

    def showAlert(message: String, alertType: Alert.AlertType): Unit = {
        val alert = new Alert(alertType, message, ButtonType.OK)
        alert.showAndWait()
    }
}
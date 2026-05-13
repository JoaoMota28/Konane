import javafx.application.Platform
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.Button
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import scala.jdk.CollectionConverters.*

class MainController {

  @FXML private var botButton: Button = _
  @FXML private var multiButton: Button = _
  @FXML private var optionsButton: Button = _
  @FXML private var loadButton: Button = _
  @FXML private var exitButton: Button = _

  private var selectedDifficulty: String = "facil"
  private var selectedBoardSize: Int = 8
  private var selectedTempo: Int = 60
  private var primaryStage: Stage = _

  def setPrimaryStage(s: Stage): Unit = {
    primaryStage = s
  }

  /**
   * Load FXML and get (root, controller) pair for navigation.
   */
  private def switchTo(fxmlPath: String): (Parent, Any) = {
    val loader = new FXMLLoader(getClass.getResource(fxmlPath))
    val root = loader.load[Parent]()
    (root, loader.getController[Any]())
  }

  /**
   * Switch content in the current Stage without opening a new one.
   */
  private def showScene(root: Parent): Unit = {
    primaryStage.getScene.setRoot(root)
  }

  /**
   * Return to main menu.
   */
  private def showMainMenu(): Unit = {
    val (menuRoot, menuController) = switchTo("/MainView.fxml")
    val ctrl = menuController.asInstanceOf[MainController]
    ctrl.setPrimaryStage(primaryStage)
    showScene(menuRoot)
  }

  /**
   * Start a bot game with the given color.
   */
  private def startBotGame(colorOpt: Option[Stone]): Unit = {
    val (botRoot, botController) = switchTo("/BotView.fxml")
    val ctrl = botController.asInstanceOf[BotController]
    ctrl.setOptions(selectedDifficulty, selectedTempo, selectedBoardSize, colorOpt)
    ctrl.setPrimaryStage(primaryStage)
    showScene(botRoot)
  }

  @FXML
  def initialize(): Unit = {
    botButton.setOnAction(_ => handlePlayWithBot())
    multiButton.setOnAction(_ => handleMultiplayer())
    loadButton.setOnAction(_ => handleLoadGame())
    optionsButton.setOnAction(_ => handleOptions())
    exitButton.setOnAction(_ => Platform.exit())
  }

  @FXML
  private def handleOptions(): Unit = {
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
  private def handlePlayWithBot(): Unit = {
    val (colorRoot, colorController) = switchTo("/ColorSelectionView.fxml")
    val ctrl = colorController.asInstanceOf[ColorSelectionController]
    ctrl.setOnColorSelected((colorOpt: Option[Stone]) => {
      startBotGame(colorOpt)
    })
    showScene(colorRoot)
  }

  @FXML
  private def handleMultiplayer(): Unit = {
    val (multiRoot, multiController) = switchTo("/MultiplayerView.fxml")
    val ctrl = multiController.asInstanceOf[MultiplayerController]
    ctrl.setOptions(selectedTempo, selectedBoardSize)
    ctrl.setPrimaryStage(primaryStage)
    showScene(multiRoot)
  }

  @FXML
  private def handleLoadGame(): Unit = {
    val dir = new java.io.File("saves")
    if (!dir.exists() || dir.listFiles().isEmpty) {
      val alert = new javafx.scene.control.Alert(javafx.scene.control.Alert.AlertType.INFORMATION,
        "Nenhum ficheiro de salvamento disponível.", javafx.scene.control.ButtonType.OK)
      alert.showAndWait()
    } else {
      val files = dir.listFiles().filter(_.getName.endsWith(".txt")).toList
      val choices = files.map(_.getName)

      val choice = new javafx.scene.control.ChoiceDialog(choices.head, choices.asJava)
      choice.setHeaderText("Escolha ficheiro para carregar")
      val res = choice.showAndWait()

      res.ifPresent(name => {
        val f = files.find(_.getName == name).get
        FileUtils.loadGameFromFile(f.getPath) match {
          case Some((board, rand, currentPlayer, openCoords, r, c, md, playerColorOpt, savedDifficulty)) =>
            selectedBoardSize = r
            selectedDifficulty = savedDifficulty

            val (gameRoot, gameController) = md match {
              case "HVC" => switchTo("/BotView.fxml")
              case "HVH" => switchTo("/MultiplayerView.fxml")
              case _ => switchTo("/BotView.fxml")
            }

            md match {
              case "HVC" =>
                val bc = gameController.asInstanceOf[BotController]
                bc.loadGame(board, rand, currentPlayer, openCoords, r, c, playerColorOpt, f.getPath, selectedDifficulty, selectedTempo)
                bc.setPrimaryStage(primaryStage)
              case "HVH" =>
                val mc = gameController.asInstanceOf[MultiplayerController]
                mc.loadGame(board, rand, currentPlayer, openCoords, r, c, f.getPath, selectedTempo)
                mc.setPrimaryStage(primaryStage)
              case _ => ()
            }

            showScene(gameRoot)

          case None =>
            val alert = new javafx.scene.control.Alert(javafx.scene.control.Alert.AlertType.ERROR,
              "Erro ao carregar ficheiro.", javafx.scene.control.ButtonType.OK)
            alert.showAndWait()
        }
      })
    }
  }
}
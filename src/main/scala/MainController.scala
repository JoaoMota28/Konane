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

  /**
   * Helper method to open a Stage with consistent configuration.
   */
  private def openStage(root: Parent, title: String, w: Int = 900, h: Int = 750, resizable: Boolean = true): Unit = {
    val stage = new Stage()
    stage.setScene(new Scene(root, w, h))
    stage.setTitle(title)
    stage.setResizable(resizable)
    stage.show()
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
    val loader = new FXMLLoader(getClass.getResource("/OptionsView.fxml"))
    val root = loader.load[Parent]()
    val controller = loader.getController[OptionsController]

    controller.setInitialOptions(selectedDifficulty, selectedBoardSize, selectedTempo)

    val stage = new Stage()
    stage.setScene(new Scene(root, 500, 450))
    stage.setTitle("Opções")
    stage.setResizable(false)
    stage.showAndWait()

    val (diff, size, tempo) = controller.getSelectedOptions
    selectedDifficulty = diff
    selectedBoardSize = size
    selectedTempo = tempo
  }

  @FXML
  private def handlePlayWithBot(): Unit = {
    // Show color selection dialog
    val colorLoader = new FXMLLoader(getClass.getResource("/ColorSelectionView.fxml"))
    val colorRoot = colorLoader.load[Parent]()
    val colorController = colorLoader.getController[ColorSelectionController]

    val colorStage = new Stage()
    colorStage.setScene(new Scene(colorRoot, 400, 300))
    colorStage.setTitle("Escolha a sua cor")
    colorStage.setResizable(false)
    colorStage.showAndWait()

    val selectedColor = colorController.getSelectedColor

    // Now load the bot game with the selected color
    val loader = new FXMLLoader(getClass.getResource("/BotView.fxml"))
    val root = loader.load[Parent]()
    val controller = loader.getController[BotController]

     controller.setOptions(selectedDifficulty, selectedTempo, selectedBoardSize, selectedColor)

     openStage(root, "Konane - Jogar contra o Computador")
   }

  @FXML
  private def handleMultiplayer(): Unit = {
    val loader = new FXMLLoader(getClass.getResource("/MultiplayerView.fxml"))
    val root = loader.load[Parent]()
    val controller = loader.getController[MultiplayerController]

     controller.setOptions(selectedTempo, selectedBoardSize)

     openStage(root, "Konane - Multiplayer")
   }

  @FXML
  private def handleLoadGame(): Unit = {
    val dir = new java.io.File("saves")
    if (!dir.exists() || dir.listFiles().isEmpty) {
      val alert = new javafx.scene.control.Alert(javafx.scene.control.Alert.AlertType.INFORMATION,
        "Nenhum ficheiro de salvamento disponível.", javafx.scene.control.ButtonType.OK)
      alert.showAndWait()
      return
    }

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
          val loader = md match {
            case "HVC" => new FXMLLoader(getClass.getResource("/BotView.fxml"))
            case "HVH" => new FXMLLoader(getClass.getResource("/MultiplayerView.fxml"))
            case _ => new FXMLLoader(getClass.getResource("/BotView.fxml"))
          }
          val root = loader.load[Parent]()

          md match {
            case "HVC" =>
              val bc = loader.getController[BotController]()
              bc.loadGame(board, rand, currentPlayer, openCoords, r, c, playerColorOpt, f.getPath, selectedDifficulty, selectedTempo)
            case "HVH" =>
              val mc = loader.getController[MultiplayerController]()
              mc.loadGame(board, rand, currentPlayer, openCoords, r, c, f.getPath, selectedTempo)
             case _ => ()
           }

           openStage(root, "Konane - Jogo Carregado")
        case None =>
          val alert = new javafx.scene.control.Alert(javafx.scene.control.Alert.AlertType.ERROR,
            "Erro ao carregar ficheiro.", javafx.scene.control.ButtonType.OK)
          alert.showAndWait()
      }
    })
  }
}
import javafx.fxml.FXML
import javafx.scene.control.Button
import javafx.stage.Stage
import KonaneLogic.*

class ColorSelectionController {

  @FXML private var blackButton: Button = _
  @FXML private var whiteButton: Button = _
  @FXML private var randomButton: Button = _

  private var selectedColor: Option[Stone] = None

  @FXML
  def initialize(): Unit = {
    blackButton.setOnAction(_ => selectColor(Some(Stone.Black)))
    whiteButton.setOnAction(_ => selectColor(Some(Stone.White)))
    randomButton.setOnAction(_ => selectColor(None))
  }

  private def selectColor(color: Option[Stone]): Unit = {
    selectedColor = color
    val stage = blackButton.getScene.getWindow.asInstanceOf[Stage]
    stage.close()
  }

  def getSelectedColor: Option[Stone] = selectedColor
}


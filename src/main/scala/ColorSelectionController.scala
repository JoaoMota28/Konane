import javafx.fxml.FXML
import javafx.scene.control.Button

class ColorSelectionController {

  @FXML private var blackButton: Button = _
  @FXML private var whiteButton: Button = _
  @FXML private var randomButton: Button = _

  private var onColorSelected: Option[Stone] => Unit = _ => ()

  @FXML
  def initialize(): Unit = {
    blackButton.setOnAction(_ => selectColor(Some(Stone.Black)))
    whiteButton.setOnAction(_ => selectColor(Some(Stone.White)))
    randomButton.setOnAction(_ => selectColor(None))
  }

  def setOnColorSelected(f: Option[Stone] => Unit): Unit = {
    onColorSelected = f
  }

  private def selectColor(color: Option[Stone]): Unit = {
    onColorSelected(color)
  }
}


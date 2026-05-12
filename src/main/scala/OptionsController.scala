import javafx.fxml.FXML
import javafx.scene.control.ComboBox
import javafx.stage.Stage

class OptionsController {

  @FXML private var difficultyBox: ComboBox[String] = _
  @FXML private var boardSizeBox: ComboBox[String] = _
  @FXML private var tempoBox: ComboBox[String] = _
  @FXML private var applyButton: javafx.scene.control.Button = _
  @FXML private var cancelButton: javafx.scene.control.Button = _

  @FXML
  def initialize(): Unit = {
    difficultyBox.getItems.addAll("facil", "medio", "dificil")
    difficultyBox.setValue("facil")

    boardSizeBox.getItems.addAll("6", "8", "10", "12")
    boardSizeBox.setValue("8")

    tempoBox.getItems.addAll("10", "30", "60", "120")
    tempoBox.setValue("60")

    applyButton.setOnAction(_ => handleSave())
    cancelButton.setOnAction(_ => handleCancel())
  }

  def setInitialOptions(difficulty: String, boardSize: Int, tempo: Int): Unit = {
    difficultyBox.setValue(difficulty)
    boardSizeBox.setValue(boardSize.toString)
    tempoBox.setValue(tempo.toString)
  }

  def getSelectedOptions: (String, Int, Int) = {
    (
      difficultyBox.getValue,
      boardSizeBox.getValue.toInt,
      tempoBox.getValue.toInt
    )
  }

  @FXML
  private def closeWindow(): Unit = {
    val stage = difficultyBox.getScene.getWindow.asInstanceOf[Stage]
    stage.close()
  }

  // Both buttons use the same handler - user changes are already reflected in the ComboBoxes
  @FXML
  private def handleSave(): Unit = closeWindow()

  @FXML
  private def handleCancel(): Unit = closeWindow()
}


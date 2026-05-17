import javafx.fxml.FXML
import javafx.scene.control.ComboBox

class OptionsController {

    @FXML var difficultyBox: ComboBox[String] = _
    @FXML var boardSizeBox: ComboBox[String] = _
    @FXML var tempoBox: ComboBox[String] = _
    @FXML var applyButton: javafx.scene.control.Button = _
    @FXML var cancelButton: javafx.scene.control.Button = _

    var onClose: () => Unit = () => ()

    @FXML
    def initialize(): Unit = {
        difficultyBox.getItems.addAll("facil", "medio", "dificil")
        difficultyBox.setValue("facil")

        boardSizeBox.getItems.addAll("6", "8", "10", "12")
        boardSizeBox.setValue("8")

        tempoBox.getItems.addAll("10", "30", "60", "120")
        tempoBox.setValue("60")

        applyButton.setOnAction(_ => closeWindow())
        cancelButton.setOnAction(_ => closeWindow())
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

    def setOnClose(f: () => Unit): Unit = {
        onClose = f
    }

    @FXML
    private def closeWindow(): Unit = {
        onClose()
    }
}


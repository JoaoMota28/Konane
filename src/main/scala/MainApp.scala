import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.Scene
import javafx.scene.Parent
import javafx.stage.Stage

class MainApp extends Application {
  override def start(primaryStage: Stage): Unit = {
    val loader = new FXMLLoader(getClass.getResource("/MainView.fxml"))
    val root: Parent = loader.load()
    val scene = new Scene(root, 500, 450)
    primaryStage.setScene(scene)
    primaryStage.setTitle("Konane")
    primaryStage.setResizable(false)
    primaryStage.show()
  }
}

object MainApp {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[MainApp], args: _*)
  }
}

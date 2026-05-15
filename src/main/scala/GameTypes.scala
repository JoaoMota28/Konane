// ADTs for game actions and results
sealed trait GameAction
case class MakeMove(from: Coord2D, to: Coord2D) extends GameAction
case class ContinueCapture(choice: Coord2D) extends GameAction
case object StopCapture extends GameAction
case object Undo extends GameAction
case object RandomMove extends GameAction
case class Save(fileName: String) extends GameAction

sealed trait TurnResult
case class MoveOk(state: GameState) extends TurnResult
case class CaptureRequired(state: GameState, lastPos: Coord2D, options: List[Coord2D]) extends TurnResult
case class InvalidAction(msg: String) extends TurnResult
case class GameOver(winner: Stone) extends TurnResult
case class SaveRequested(board: Board, rand: MyRandom, currentPlayer: Stone, openCoords: List[Coord2D], rows: Int, cols: Int, mode: String, playerColorOpt: Option[Stone], difficulty: String, fileName: String, history: List[(Board, MyRandom, Stone, List[Coord2D])]) extends TurnResult

// Immutable game state
// Note: Information about human vs computer players is derived from mode + playerColorOpt
// No redundant blackPlayer/whitePlayer fields - avoid inconsistent state
case class GameState(
                      board: Board,
                      rand: MyRandom,
                      currentPlayer: Stone,
                      openCoords: List[Coord2D],
                      rows: Int,
                      cols: Int,
                      history: List[(Board, MyRandom, Stone, List[Coord2D])],
                      mode: String,
                      playerColorOpt: Option[Stone],
                      timeLimitMillis: Long,
                      difficulty: String,
                      pendingCapture: Option[Coord2D],
                      captureSequenceStartState: Option[(Board, MyRandom, Stone, List[Coord2D])] = None
                    )
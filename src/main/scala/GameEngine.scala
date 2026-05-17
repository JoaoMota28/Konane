import KonaneLogic.*
import Difficulty.makeChooser

object GameEngine {

    def switchPlayer(p: Stone): Stone = if (p == Stone.Black) Stone.White else Stone.Black

    def finishTurn(state: GameState, nb: Board, no: List[Coord2D], prevPlayer: Stone,
                   newHistory: List[(Board, MyRandom, Stone, List[Coord2D])]): (GameState, TurnResult) = {
        val nextPlayer = switchPlayer(prevPlayer)
        val newState   = state.copy(board = nb, openCoords = no, currentPlayer = nextPlayer,
            history = newHistory, pendingCapture = None)
        getWinner(nb, nextPlayer, newState.rows, newState.cols) match {
            case Some(winner) => (newState, GameOver(winner))
            case None         => (newState, MoveOk(newState))
        }
    }

    def handleMakeMove(state: GameState, from: Coord2D, to: Coord2D): (GameState, TurnResult) =
        play(state.board, state.currentPlayer, from, to, state.openCoords) match {
            case (None, _)       => (state, InvalidAction("Jogada invalida"))
            case (Some(nb), no)  =>
                val possible = getValidMovesForPiece(nb, state.currentPlayer, to, state.rows, state.cols)
                if (possible.isEmpty) {
                    val newHistory = (state.board, state.rand, state.currentPlayer, state.openCoords) :: state.history
                    finishTurn(state, nb, no, state.currentPlayer, newHistory)
                } else {
                    val newState = state.copy(board = nb, openCoords = no, pendingCapture = Some(to),
                        captureSequenceStartState = Some((state.board, state.rand, state.currentPlayer, state.openCoords)))
                    (newState, CaptureRequired(newState, to, possible))
                }
        }

    def handleContinueCapture(state: GameState, choice: Coord2D): (GameState, TurnResult) =
        state.pendingCapture match {
            case None => (state, InvalidAction("Nenhuma captura pendente"))
            case Some(lastPos) =>
                continueCapturePure(state.board, state.currentPlayer, lastPos, Some(choice), state.rows, state.cols, state.openCoords) match {
                    case None => (state, InvalidAction("Jogada invalida para captura"))
                    case Some((b2, o2, canContinue)) =>
                        if (canContinue) {
                            val possible = getValidMovesForPiece(b2, state.currentPlayer, choice, state.rows, state.cols)
                            val newState = state.copy(board = b2, openCoords = o2, pendingCapture = Some(choice))
                            (newState, CaptureRequired(newState, choice, possible))
                        } else {
                            val (historyBoard, historyRand, historyPlayer, historyOpen) =
                                state.captureSequenceStartState.getOrElse((state.board, state.rand, state.currentPlayer, state.openCoords))
                            val newHistory = (historyBoard, historyRand, historyPlayer, historyOpen) :: state.history
                            finishTurn(state, b2, o2, state.currentPlayer, newHistory)
                        }
                }
        }

    def handleRandomMove(state: GameState): (GameState, TurnResult) = {
        val chooser = makeChooser(state.difficulty, state.board, state.currentPlayer, state.rows, state.cols, state.openCoords, state.mode)
        val (newBoardOpt, nextRand, nextOpen, moveOpt) =
            playRandomly(state.board, state.rand, state.currentPlayer, state.openCoords, chooser)
        moveOpt match {
            case None => (state, InvalidAction("Nenhuma jogada possivel"))
            case Some(to) =>
                val boardNow  = newBoardOpt.get
                val tempState = state.copy(board = boardNow, rand = nextRand, openCoords = nextOpen)
                val possible  = getValidMovesForPiece(boardNow, state.currentPlayer, to, state.rows, state.cols)

                val isHumanTurn = state.playerColorOpt.contains(state.currentPlayer) || state.mode == "HVH"
                val shouldContinueCapture = if (isHumanTurn) possible.nonEmpty
                else state.difficulty.toLowerCase match {
                    case "facil" => false
                    case _       => possible.nonEmpty
                }

                if (possible.isEmpty || !shouldContinueCapture) {
                    val newHistory = (state.board, state.rand, state.currentPlayer, state.openCoords) :: state.history
                    val nextPlayer = switchPlayer(state.currentPlayer)
                    val newState   = tempState.copy(currentPlayer = nextPlayer, history = newHistory)
                    getWinner(boardNow, nextPlayer, newState.rows, newState.cols) match {
                        case Some(winner) => (newState, GameOver(winner))
                        case None         => (newState, MoveOk(newState))
                    }
                } else {
                    val newState = tempState.copy(pendingCapture = Some(to),
                        captureSequenceStartState = Some((state.board, state.rand, state.currentPlayer, state.openCoords)))
                    (newState, CaptureRequired(newState, to, possible))
                }
        }
    }

    def handleStopCapture(state: GameState): (GameState, TurnResult) = {
        val (historyBoard, historyRand, historyPlayer, historyOpen) =
            state.captureSequenceStartState.getOrElse((state.board, state.rand, state.currentPlayer, state.openCoords))
        val newHistory = (historyBoard, historyRand, historyPlayer, historyOpen) :: state.history
        finishTurn(state, state.board, state.openCoords, state.currentPlayer, newHistory)
    }

    def handleUndo(state: GameState): (GameState, TurnResult) = {
        if (state.history.isEmpty) return (state, MoveOk(state))
        state.mode match {
            case "HVC" =>
                val humanColor = state.playerColorOpt.getOrElse(Stone.Black)
                val idx = state.history.indexWhere { case (_, _, p, _) => p == humanColor }
                if (idx < 0) (state, MoveOk(state))
                else {
                    val (b0, r0, p0, open0) = state.history(idx)
                    val newState = state.copy(board = b0, rand = r0, currentPlayer = p0,
                        openCoords = open0, history = state.history.drop(idx + 1),
                        pendingCapture = None, captureSequenceStartState = None)
                    (newState, MoveOk(newState))
                }
            case _ =>
                val (b0, r0, p0, open0) = state.history.head
                val newState = state.copy(board = b0, rand = r0, currentPlayer = p0,
                    openCoords = open0, history = state.history.tail,
                    pendingCapture = None, captureSequenceStartState = None)
                (newState, MoveOk(newState))
        }
    }

    def handleSave(state: GameState, fileName: String): (GameState, TurnResult) =
        (state, SaveRequested(state.board, state.rand, state.currentPlayer, state.openCoords,
            state.rows, state.cols, state.mode, state.playerColorOpt,
            state.difficulty, fileName, state.history))

    def handleAction(state: GameState, action: GameAction): (GameState, TurnResult) = action match {
        case MakeMove(from, to)    => handleMakeMove(state, from, to)
        case ContinueCapture(choice) => handleContinueCapture(state, choice)
        case StopCapture           => handleStopCapture(state)
        case RandomMove            => handleRandomMove(state)
        case Undo                  => handleUndo(state)
        case Save(fileName)        => handleSave(state, fileName)
    }
}
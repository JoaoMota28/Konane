import KonaneLogic.*

object GameEngine {
  // Direct aliases to KonaneLogic (game rules)
  def initBoard(rows: Int, cols: Int): Board = KonaneLogic.initBoard(rows, cols)
  def setupBoard(board: Board, rows: Int, cols: Int, rand: MyRandom): (Board, List[Coord2D], MyRandom) = KonaneLogic.setupBoard(board, rows, cols, rand)
  def isValidDimension(r: Int, c: Int): Boolean = KonaneLogic.isValidDimension(r, c)
  def isValidMove(board: Board, player: Stone, from: Coord2D, to: Coord2D, rows: Int, cols: Int): Boolean = KonaneLogic.isValidMove(board, player, from, to, rows, cols)
  def play(board: Board, player: Stone, coordFrom: Coord2D, coordTo: Coord2D, lstOpenCoords: List[Coord2D]): (Option[Board], List[Coord2D]) = KonaneLogic.play(board, player, coordFrom, coordTo, lstOpenCoords)
  def getValidMovesForPiece(board: Board, player: Stone, from: Coord2D, rows: Int, cols: Int): List[Coord2D] = KonaneLogic.getValidMovesForPiece(board, player, from, rows, cols)
  def getAllValidMoves(board: Board, player: Stone, rows: Int, cols: Int): List[(Coord2D, Coord2D)] = KonaneLogic.getAllValidMoves(board, player, rows, cols)
  def playRandomly(board: Board, r: MyRandom, player: Stone, lstOpenCoords: List[Coord2D], f: (List[Coord2D], MyRandom) => (Coord2D, MyRandom)): (Option[Board], MyRandom, List[Coord2D], Option[Coord2D]) = KonaneLogic.playRandomly(board, r, player, lstOpenCoords, f)
  def boardToString(board: Board, rows: Int, cols: Int): String = KonaneLogic.boardToString(board, rows, cols)
  def parseInput(s: String): Option[Coord2D] = KonaneLogic.parseInput(s)
  def coordToString(c: Coord2D): String = KonaneLogic.coordToString(c)
  def getWinner(board: Board, currentPlayer: Stone, rows: Int, cols: Int): Option[Stone] = KonaneLogic.getWinner(board, currentPlayer, rows, cols)
  def continueCapturePure(board: Board, player: Stone, lastPos: Coord2D, choiceOpt: Option[Coord2D], rows: Int, cols: Int, openCoords: List[Coord2D]): Option[(Board, List[Coord2D], Boolean)] = KonaneLogic.continueCapturePure(board, player, lastPos, choiceOpt, rows, cols, openCoords)
  def serializeGame(board: Board, randSeed: Long, currentPlayer: Stone, openCoords: List[Coord2D], rows: Int, cols: Int, mode: String, playerColorOpt: Option[Stone], difficulty: String, history: List[(Board, MyRandom, Stone, List[Coord2D])]): String = KonaneLogic.serializeGame(board, randSeed, currentPlayer, openCoords, rows, cols, mode, playerColorOpt, difficulty, history)
  def parseGameContent(content: String): Option[(Board, MyRandom, Stone, List[Coord2D], Int, Int, String, Option[Stone], String, List[(Board, MyRandom, Stone, List[Coord2D])])] = KonaneLogic.parseGameContent(content)


  // --- Pure game controller helpers (moved here to keep a single pure core) ---
  private def switchPlayer(p: Stone): Stone = if (p == Stone.Black) Stone.White else Stone.Black

  // After a turn ends, switch player and check win condition.
  private def finishTurn(state: GameState, nb: Board, no: List[Coord2D], prevPlayer: Stone, newHistory: List[(Board, MyRandom, Stone, List[Coord2D])]): (GameState, TurnResult) = {
    val nextPlayer = switchPlayer(prevPlayer)
    val newState = state.copy(board = nb, openCoords = no, currentPlayer = nextPlayer, history = newHistory, pendingCapture = None)
    getWinner(nb, nextPlayer, newState.rows, newState.cols) match {
      case Some(winner) => (newState, GameOver(winner))
      case None         => (newState, MoveOk(newState))
    }
  }

  def handleMakeMove(state: GameState, from: Coord2D, to: Coord2D): (GameState, TurnResult) = {
    val gs = state
    KonaneLogic.play(gs.board, gs.currentPlayer, from, to, gs.openCoords) match {
      case (Some(nb), no) => Some((nb, no))
      case _ => None
    } match {
      case None => (gs, InvalidAction("Jogada invalida"))
      case Some((nb, no)) =>
        val possible = getValidMovesForPiece(nb, gs.currentPlayer, to, gs.rows, gs.cols)
        if (possible.isEmpty) {
          // No continuation possible — end turn, check win
          val newHistory = (gs.board, gs.rand, gs.currentPlayer, gs.openCoords) :: gs.history
          finishTurn(gs, nb, no, gs.currentPlayer, newHistory)
        } else {
          // Continuation possible — always ask the player (even if only one option)
          // Save the pre-capture state for later history update
          val newState = gs.copy(board = nb, openCoords = no, pendingCapture = Some(to), captureSequenceStartState = Some((gs.board, gs.rand, gs.currentPlayer, gs.openCoords)))
          (newState, CaptureRequired(newState, to, possible))
        }
    }
  }

  def handleContinueCapture(state: GameState, choice: Coord2D): (GameState, TurnResult) = {
    state.pendingCapture match {
      case None => (state, InvalidAction("Nenhuma captura pendente"))
      case Some(lastPos) =>
        continueCapturePure(state.board, state.currentPlayer, lastPos, Some(choice), state.rows, state.cols, state.openCoords) match {
          case None => (state, InvalidAction("Jogada invalida para captura"))
          case Some((b2, o2, canContinue)) =>
            if (canContinue) {
              val possible = getValidMovesForPiece(b2, state.currentPlayer, choice, state.rows, state.cols)
              val newState = state.copy(board = b2, openCoords = o2, pendingCapture = Some(choice))
              // Always prompt the player, even if only one continuation exists
              (newState, CaptureRequired(newState, choice, possible))
            } else {
              // No more captures possible — end turn, check win
              // Use saved pre-capture state for history, or fall back to current state if not tracking
              val (historyBoard, historyRand, historyPlayer, historyOpen) = state.captureSequenceStartState.getOrElse((state.board, state.rand, state.currentPlayer, state.openCoords))
              val newHistory = (historyBoard, historyRand, historyPlayer, historyOpen) :: state.history
              finishTurn(state, b2, o2, state.currentPlayer, newHistory)
            }
        }
    }
  }

  def handleRandomMove(state: GameState): (GameState, TurnResult) = {
    val chooser = makeChooser(state.difficulty, state.board, state.currentPlayer, state.rows, state.cols, state.openCoords, state.mode)
    val (newBoardOpt, nextRand, nextOpen, moveOpt) = playRandomly(state.board, state.rand, state.currentPlayer, state.openCoords, chooser)
    moveOpt match {
      case None => (state, InvalidAction("Nenhuma jogada possivel"))
      case Some(to) =>
        val boardNow = newBoardOpt.get
        val tempState = state.copy(board = boardNow, rand = nextRand, openCoords = nextOpen)
        val possible = getValidMovesForPiece(boardNow, state.currentPlayer, to, state.rows, state.cols)

        // Check if this is a human's random move (human vs computer) or any player in HVH mode
        val isHumanRandomMove = state.playerColorOpt.contains(state.currentPlayer)
        val isHVHMode = state.mode == "HVH"

        // Determine capture behavior:
        // - If human player in HVC mode: ALWAYS prompt for chain captures so they can decide
        // - If player in HVH mode: ALWAYS prompt for chain captures
        // - If computer player: respect difficulty setting
        val shouldContinueCapture = if (isHumanRandomMove || isHVHMode) {
          // Human player or HVH mode always gets prompted for chain captures
          possible.nonEmpty
        } else {
          // Computer respects difficulty setting
          state.difficulty.toLowerCase match {
            case "facil" | "easy" => false  // Easy: never do consecutive captures
            case "medio" | "medium" => possible.nonEmpty  // Medium: always do consecutive captures if possible
            case "dificil" | "hard" => possible.nonEmpty  // Hard: always do consecutive captures if possible
            case _ => possible.nonEmpty
          }
        }

        if (possible.isEmpty || !shouldContinueCapture) {
          // No continuation possible or (computer on Easy mode): end turn, check win
          val newHistory = (state.board, state.rand, state.currentPlayer, state.openCoords) :: state.history
          val nextPlayer = switchPlayer(state.currentPlayer)
          val newState = tempState.copy(currentPlayer = nextPlayer, history = newHistory)
          getWinner(boardNow, nextPlayer, newState.rows, newState.cols) match {
            case Some(winner) => (newState, GameOver(winner))
            case None         => (newState, MoveOk(newState))
          }
        } else {
          // Continuation possible — emit CaptureRequired for chain captures
          // Save the pre-capture state for history update when capture sequence ends
          val newState = tempState.copy(pendingCapture = Some(to), captureSequenceStartState = Some((state.board, state.rand, state.currentPlayer, state.openCoords)))
          (newState, CaptureRequired(newState, to, possible))
        }
    }
  }

  def handleStopCapture(state: GameState): (GameState, TurnResult) = {
    // Player chose to stop capturing — end turn now, check win
    // Use saved pre-capture state for history, or fall back to current state if not tracking
    val (historyBoard, historyRand, historyPlayer, historyOpen) = state.captureSequenceStartState.getOrElse((state.board, state.rand, state.currentPlayer, state.openCoords))
    val newHistory = (historyBoard, historyRand, historyPlayer, historyOpen) :: state.history
    finishTurn(state, state.board, state.openCoords, state.currentPlayer, newHistory)
  }

  def handleUndo(state: GameState): (GameState, TurnResult) = {
    if (state.history.isEmpty) return (state, MoveOk(state))

    state.mode match {
      case "HVC" =>
        // In HVC we must always restore to a state where it's the human's turn.
        // Walk the history (newest-first) and find the first entry whose player
        // matches the human's colour.  Remove every entry up to and including
        // that entry so the history stays consistent.
        val humanColor = state.playerColorOpt.getOrElse(Stone.Black)
        val idx = state.history.indexWhere { case (_, _, p, _) => p == humanColor }
        if (idx < 0) (state, MoveOk(state)) // no human-turn entry found – nothing to undo
        else {
          val (b0, r0, p0, open0) = state.history(idx)
          val newHistory = state.history.drop(idx + 1)
          val newState = state.copy(board = b0, rand = r0, currentPlayer = p0,
            openCoords = open0, history = newHistory,
            pendingCapture = None, captureSequenceStartState = None)
          (newState, MoveOk(newState))
        }

      case _ =>
        // HVH (and any other mode): undo exactly one turn
        val (b0, r0, p0, open0) = state.history.head
        val newHistory = state.history.tail
        val newState = state.copy(board = b0, rand = r0, currentPlayer = p0,
          openCoords = open0, history = newHistory,
          pendingCapture = None, captureSequenceStartState = None)
        (newState, MoveOk(newState))
    }
  }

  def handleSave(state: GameState, fileName: String): (GameState, TurnResult) = {
    (state, SaveRequested(state.board, state.rand, state.currentPlayer, state.openCoords, state.rows, state.cols, state.mode, state.playerColorOpt, state.difficulty, fileName, state.history))
  }

  def handleAction(state: GameState, action: GameAction): (GameState, TurnResult) = action match {
    case MakeMove(from, to) => handleMakeMove(state, from, to)
    case ContinueCapture(choice) => handleContinueCapture(state, choice)
    case StopCapture => handleStopCapture(state)
    case RandomMove => handleRandomMove(state)
    case Undo => handleUndo(state)
    case Save(fileName) => handleSave(state, fileName)
  }
}
package models

class Board {

  var state: Array[Array[Option[Piece]]] = Array.ofDim[Option[Piece]](Board.size, Board.size)

  // Set all squares in Array to None
  for (i <- state.indices; j <- state(i).indices) state(i)(j) = None

  var piecesTaken: Array[Piece] = Array()

  def addPieceToBoard(piece: Piece, location: Position) = {
    val array = state(location.x)
    array.update(location.y, Some(piece))
    state.update(location.x, array)
    piece.currentPosition = location
  }

  def removePieceFromBoard(takenPiece: Piece) = {
    piecesTaken = piecesTaken :+ takenPiece
    takenPiece.currentPosition = Position(-1, -1)
  }

  def getKingPosition(player:Player): Position = {
    val tempArray = for {
      row <- state
      piece <- row
      if piece.orNull.isInstanceOf[King] && piece.orNull.owner == player
    } yield piece.get.currentPosition

    assert(tempArray.length == 1)
    tempArray.head
  }

  def getAllPieces: Array[Piece] = state.flatten.flatten


  def updateBoard(from: Position, to: Position) = {
    val activePiece = state(from.x)(from.y).getOrElse(throw new NoSuchElementException("Active Piece not retrievable after validation"))
    state(from.x)(from.y) = None
    val takenPiece = state(to.x)(to.y)
    if (takenPiece.isDefined) removePieceFromBoard(takenPiece.get)
    state(to.x)(to.y) = Some(activePiece)
    activePiece.currentPosition = to

    activePiece match {
      case pawn: Pawn =>
        pawn.firstMoveMade = true
      case _ =>
    }
  }

  def createClone:Board = {
    val x = new Board
    x.state = this.state.map(_.map({
        case Some(piece) => Some(piece.clone)
        case None => None
    }))
    x.piecesTaken = this.piecesTaken.map(_.clone)
    x
  }
}

object Board {
  val size = 8
}

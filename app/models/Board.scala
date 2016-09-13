package models

class Board {

  var state: Array[Array[Option[Piece]]] = Array.ofDim[Option[Piece]](Board.size, Board.size)

  // Set all squares in Array to None
  for {
    i <- state.indices
    j <- state(i).indices
  } state(i)(j) = None

  var piecesTaken: Array[Piece] = Array()

  def addPieceToBoard(piece: Piece, location: (Int, Int)) = {
    val array = state(location._1)
    array.update(location._2, Some(piece))
    state.update(location._1, array)
    piece.currentPosition = location
  }

  def removePieceFromBoard(takenPiece: Piece) = {
    piecesTaken = piecesTaken :+ takenPiece
    takenPiece.currentPosition = (-1, -1)
  }

  def getKingPosition(player:Player): (Int, Int) = {
    val tempArray = for {
      row <- state
      piece <- row
      if piece.orNull.isInstanceOf[King] && piece.orNull.owner == player
    } yield piece.get.currentPosition

    assert(tempArray.length == 1)
    tempArray.head
  }

  def getAllPieces: Array[Piece] = state.flatten.flatten


  def updateBoard(from: (Int, Int), to: (Int, Int)) = {
    val activePiece = state(from._1)(from._2).getOrElse(throw new NoSuchElementException("Active Piece not retrievable after validation"))
    state(from._1)(from._2) = None
    val takenPiece = state(to._1)(to._2)
    if (takenPiece.isDefined) removePieceFromBoard(takenPiece.get)
    state(to._1)(to._2) = Some(activePiece)
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

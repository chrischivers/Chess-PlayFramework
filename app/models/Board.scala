package models


class Board {

  val state = Array.ofDim[Piece](Board.size, Board.size)
  var piecesTaken: Array[Piece] = Array()

  def addPieceToBoard(piece: Piece, location: (Int, Int)) = {
    val array = state(location._1)
    array.update(location._2, piece)
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
      if piece.isInstanceOf[King] && piece.owner == player
    } yield piece.currentPosition

    assert(tempArray.length == 1)
    tempArray.head
  }

  def getAllPieces: Array[Piece] = for {
      x <- state
      piece <- x
      if piece != null
    } yield piece


  def updateBoard(from: (Int, Int), to: (Int, Int)) = {
    val activePiece = state(from._1)(from._2)
    state(from._1)(from._2) = null
    val takenPiece = state(to._1)(to._2)
    if (takenPiece != null) removePieceFromBoard(takenPiece)
    state(to._1)(to._2) = activePiece
    activePiece.currentPosition = (to._1, to._2)

    activePiece match {
      case pawn: Pawn =>
        pawn.firstMoveMade = true
      case _ =>
    }
    assert(activePiece != null)
  }
}

object Board {
  val size = 8
}

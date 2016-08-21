package models


class Board {

  val state = Array.ofDim[Piece](Board.size, Board.size)
  var piecesTaken: Array[Piece] = Array()

   def addPieceToBoard(piece:Piece, location:(Int,Int)) = {
    val array = state(location._1)
    array.update(location._2, piece)
    state.update(location._1, array)
  }

  def updateBoard(from: (Int, Int), to: (Int, Int)) = {
    val activePiece = state(from._1)(from._2)
    state(from._1)(from._2) = null
    val takenPiece = state(to._1)(to._2)

    if (takenPiece != null) {
      piecesTaken = piecesTaken :+ takenPiece
    }
    state(to._1)(to._2) = activePiece

    activePiece match {
      case pawn: Pawn =>
        pawn.firstMoveMade = true
    }
    assert(activePiece != null)
      }
}

object Board {
  val size = 8
}

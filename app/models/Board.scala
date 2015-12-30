package models


class Board {

  val state = Array.ofDim[Piece](8, 8)

   def addPieceToBoard(piece:Piece, location:(Int,Int)) = {
    val array = state(location._1)
    array.update(location._2, piece)
    state.update(location._1, array)
  }
}

object Board {
  val size = 8
}

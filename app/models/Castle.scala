package models


class Castle(var owner:Player) extends Piece {
  override def canPiecePerformMove(from: (Int, Int), to: (Int, Int)): Boolean = {
    from._1 == to._1 ^ from._2 == to._2
  }

  override val pieceName: String = "Castle"
}
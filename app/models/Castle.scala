package models


class Castle(var ownedBy:Player) extends Piece {
  override def isMoveValid(from: (Int, Int), to: (Int, Int)): Unit = ???

  override val pieceName: String = "Castle"
}
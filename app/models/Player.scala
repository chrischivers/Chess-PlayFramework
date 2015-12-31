package models

trait Player {
  val playerName:String
  val yDirection:Int //-1 for moving up board, 1 for moving down board
  override def toString:String = playerName
}

case class Player1 (playerName:String) extends Player {
  override val yDirection: Int = -1
}

case class Player2 (playerName:String) extends Player {
  override val yDirection: Int = 1
}

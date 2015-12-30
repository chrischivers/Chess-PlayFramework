package models

/**
 * Created by chrischivers on 29/12/15.
 */
trait Player {
  val playerName:String
  override def toString:String = playerName
}

case class Player1 (playerName:String) extends Player

case class Player2 (playerName:String) extends Player

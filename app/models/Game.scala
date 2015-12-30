package models


class Game() {

  var nextPlayerturn:Player = _
  val board: Board = Game.setUpNewBoard()

}

object Game {
  def setUpNewBoard():Board = {
    val player1 = new Player1("Player 1")
    val player2 = new Player2("Player 2")

    val castleP1 = new Castle(player1)
    val castleP2 = new Castle(player2)
    //TODO more

    val board = new Board
    board.addPieceToBoard(castleP1,(0,0))
    board.addPieceToBoard(castleP2,(0,7))
    board
  }
}

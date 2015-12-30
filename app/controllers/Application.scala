package controllers

import models.{Player2, Player1, Game}
import play.api.mvc._


object Application extends Controller {

  private var activeGames:Map[String, Game] = Map()

  val p1 = new Player1("P1")
  val p2 = new Player2("P2")

  def index = Action {
    val game = new Game(p1,p2)
    println("adding game ID: " + game.gameID)
    activeGames += (game.gameID -> game)
    Ok(views.html.main(game.gameID))
  }

  def processMove(gameID:String, from:String, to:String) = Action {
    val moveFromSplit = from.split(",").map(_.toInt)
    val moveFrom = (moveFromSplit(0), moveFromSplit(1))
    val moveToSplit = to.split(",").map(_.toInt)
    val moveTo = (moveToSplit(0), moveToSplit(1))

    val gameOpt = activeGames.get(gameID.trim)
    if (gameOpt.isDefined) {
      val game = gameOpt.get
      println("Piece is: " + game.board.state(moveFrom._1)(moveFrom._2))
      if (game.isMoveValid(moveFrom,moveTo)) {
        println("Move valid")
        game.updateBoard(moveFrom,moveTo)
        println("Board updated")
        Ok(views.html.board(game)).withHeaders(
          ("RESULT", "SUCCESS"))
      } else {
        println("Move invalid")
        Ok("N/A").withHeaders(
          ("RESULT","INVALID_MOVE")
        )
      }
    } else {
      println("Not an active game")
      Ok("N/A").withHeaders(
        ("RESULT","INVALID_GAME")
      )
    }
  }

  def getBoard(gameID: String) = Action {
    val gameOpt = activeGames.get(gameID.trim)
    if (gameOpt.isDefined) {
      val game = gameOpt.get
      Ok(views.html.board(game)).withHeaders(
        ("RESULT", "SUCCESS"))
    } else {
      NotImplemented
    }
  }
}

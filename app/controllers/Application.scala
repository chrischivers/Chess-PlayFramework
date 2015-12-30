package controllers

import models.Game
import play.api.data.Form
import play.api.mvc._

case class Move(gameID: String, moveFrom: String, moveTo:String)

object Application extends Controller {

  import play.api.data._
  import play.api.data.Forms._


  private var activeGames:Map[String, Game] = Map()

  def index = Action {
    val game = new Game()
    activeGames = activeGames + (game.gameID -> game)
    Ok(views.html.main(game.gameID))
  }

  def processMove(gameID:String, from:String, to:String) = Action {
    println("here")
    val moveFromSplit = from.split(",").map(_.toInt)
    val moveFrom = (moveFromSplit(0), moveFromSplit(1))
    val moveToSplit = to.split(",").map(_.toInt)
    val moveTo = (moveToSplit(0), moveToSplit(1))
    val gameOpt = activeGames.get(gameID)
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
    val gameOpt = activeGames.get(gameID)
    if (gameOpt.isDefined) {
      val game = gameOpt.get
      Ok(views.html.board(game)).withHeaders(
        ("RESULT", "SUCCESS"))
    } else {
      NotImplemented
    }
  }


  def addActiveGame(game:Game) = {
    activeGames = activeGames + (game.gameID -> game)
  }


}

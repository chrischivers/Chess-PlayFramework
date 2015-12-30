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
    Ok(views.html.main(new Game()))
  }

  def processMove(gameID:String, from:String, to:String) = Action {
    println("here")
    val moveFromSplt = from.split(",").map(_.toInt)
    val moveToSplt = to.split(",").map(_.toInt)
    val game = activeGames.get(gameID)
    if (game.isDefined) {
      println("Piece is: " + game.get.board.state(moveFromSplt(0))(moveFromSplt(1)))
    } else {
      println("Not an active game")
    }
    Ok("response here")
  }


  def addActiveGame(game:Game) = {
    activeGames = activeGames + (game.gameID -> game)
  }


}

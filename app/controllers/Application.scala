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
    val moveFromSplit = from.split(",").map(_.toInt)
    val moveFrom = (moveFromSplit(0), moveFromSplit(1))
    val moveToSplit = to.split(",").map(_.toInt)
    val moveTo = (moveToSplit(0), moveToSplit(1))
    val game = activeGames.get(gameID)
    if (game.isDefined) {
      println("Piece is: " + game.get.board.state(moveFrom._1)(moveFrom._2))
      if (game.get.isMoveValid(moveFrom,moveTo)) {
        println("Move valid")
        game.get.updateBoard(moveFrom,moveTo)
        println("Board updaed")
      } else {
        println("Move invalid")
      }
    } else {
      println("Not an active game")
    }
    Ok("response here")
  }


  def addActiveGame(game:Game) = {
    activeGames = activeGames + (game.gameID -> game)
  }


}

package controllers

import models.Game
import play.api.mvc._

object Application extends Controller {

  def index = Action {
    Ok(views.html.main(new Game()))
  }


}

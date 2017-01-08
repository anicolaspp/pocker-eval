/**
  * Created by anicolaspp on 1/7/17.
  */

import Implicits._
import scala.io.StdIn

object app extends App {

  val gameResult = for {
    game <- StdIn.readLine().game
  } yield game.play

  println(gameResult)
}




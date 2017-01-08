/**
  * Created by anicolaspp on 1/7/17.
  */

sealed trait GameResult
case object Tie extends GameResult
case object White  extends GameResult
case object Black extends GameResult

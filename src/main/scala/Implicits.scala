import scalaz.{-\/, \/, \/-}

/**
  * Created by anicolaspp on 1/7/17.
  */
object Implicits {

  implicit class StrExt(s: String) {
    def game: Error \/ GameInstance = Parser.parseInput(s) match {
      case -\/(a) =>  -\/(a)
      case \/-(b) =>  \/-(b)
    }
  }

  implicit def toPlayable(hands: (Hand, Hand)): GameInstance = new GameInstance(hands._1, hands._2)
}

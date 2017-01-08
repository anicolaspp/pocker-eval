/**
  * Created by anicolaspp on 1/7/17.
  */
object Implicits {

  implicit class StrExt(s: String) {
    def play = Parser.parseInput(s).play
  }

  implicit def toPlayable(hands: (Hand, Hand)): GameInstance = new GameInstance(hands._1, hands._2)
}

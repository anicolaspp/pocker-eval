/**
  * Created by anicolaspp on 1/7/17.
  */
class GameInstance(black: Hand, white: Hand) {
  def play = Hand.compare(black, white)(Chain.full)
}

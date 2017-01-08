/**
  * Created by anicolaspp on 1/7/17.
  */
object StraightEvaluator {

  def checkValues(sorted: List[Int], i: Int): Boolean = sorted match {
    case Nil    =>  true
    case x :: t =>  if (x != i) false else checkValues(t, i + 1)
  }

  def eval(hand: Hand) = {
    val sorted = hand.cards.map(_.value).sorted

    if (checkValues(sorted, sorted.head)) {
      Straight
    } else {
      HighCard
    }
  }

}

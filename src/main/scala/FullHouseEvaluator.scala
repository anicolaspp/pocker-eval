/**
  * Created by anicolaspp on 1/7/17.
  */
object FullHouseEvaluator {
  def checkValues(sorted: List[Int], i: Int): Boolean = sorted match {
    case Nil    =>  true
    case x :: t =>  if (x != i) false else checkValues(t, i + 1)
  }

  def eval(hand: Hand): HandType = {
    val groups = hand.cards.groupBy(_.value)

    if (groups.toList.length == 2) {
      FullHouse
    } else {
      HighCard
    }
  }
}

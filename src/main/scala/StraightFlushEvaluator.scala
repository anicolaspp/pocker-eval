/**
  * Created by anicolaspp on 1/7/17.
  */
object StraightFlushEvaluator {

  def checkValues(sorted: List[Int], i: Int): Boolean = sorted match {
    case Nil    =>  true
    case x :: t =>  if (x != i) false else checkValues(t, i + 1)
  }

  def eval(hand: Hand): HandType = {
    val groups = hand.cards.groupBy(_.getClass)

    if (groups.keySet.toList.length == 1) {
      val sorted = hand.cards.map(_.value).sorted

      if (checkValues(sorted, sorted.head)) {
        StraightFlush
      }
      else {
        HighCard
      }
    } else {
      HighCard
    }
  }

}

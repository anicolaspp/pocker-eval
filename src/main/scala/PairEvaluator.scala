/**
  * Created by anicolaspp on 1/7/17.
  */
object PairEvaluator {
  def eval(hand: Hand): HandType =
    if (hand.cards.groupBy(_.value).filter(_._2.length == 2).toList.nonEmpty) Pair else HighCard
}

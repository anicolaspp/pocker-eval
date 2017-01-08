/**
  * Created by anicolaspp on 1/7/17.
  */
object FlushEvaluator{
  def eval(hand: Hand): HandType =
    if (hand.cards.groupBy(_.getClass).toList.length == 1) Flush else HighCard
}

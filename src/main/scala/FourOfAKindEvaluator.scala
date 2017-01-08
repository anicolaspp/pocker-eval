/**
  * Created by anicolaspp on 1/7/17.
  */
object FourOfAKindEvaluator {
  def eval(hand: Hand): HandType =
    if (hand.cards.groupBy(_.value).filter(_._2.length == 4).toList.length == 1) FourOfAKind else HighCard

}

/**
  * Created by anicolaspp on 1/7/17.
  */
object ThreeOfAKindEvaluator {
  def eval(hand: Hand): HandType =
    if (hand.cards.groupBy(_.value).filter(_._2.length == 3).toList.length == 1) ThreeOfAKind else HighCard

}

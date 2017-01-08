/**
  * Created by anicolaspp on 1/7/17.
  */

case class Hand(cards: List[Card])
import Card._

object Hand {

  import HandType._

  def apply(cards: Card*): Hand = Hand(cards.toList)

  implicit def toHand(cards: Array[Card]): Hand = Hand(cards.toList)

  def compare(black: Hand, white: Hand)(implicit chain: Chain): (HandType, GameResult) = {
    val blackValue = chain.eval(black)
    val whiteValue = chain.eval(white)

    if (blackValue == whiteValue) {

      blackValue match {
        case HighCard => (HighCard, compareInOrder(black.cards.sorted.reverse, white.cards.sorted.reverse))
        case Pair => (Pair, comparePairs(black.cards, white.cards))
        case TwoPairs => (TwoPairs, compareTwoPair(black.cards, white.cards))
        case ThreeOfAKind => (ThreeOfAKind, compareThreeOfAKind(black.cards, white.cards))
        case Straight =>  (Straight, compareInOrder(black.cards.sorted.reverse, white.cards.sorted.reverse))
        case Flush  => (Flush, compareInOrder(black.cards.sorted.reverse, white.cards.sorted.reverse))
        case FullHouse  =>  (FullHouse, compareThreeOfAKind(black.cards, white.cards))
        case FourOfAKind  => (FourOfAKind, compareFourOfAKind(black.cards, white.cards))
        case StraightFlush => (StraightFlush, compareStraightFlush(black.cards, white.cards))
      }

    } else if (blackValue < whiteValue) {
      (blackValue, White)
    } else {
      (blackValue, Black)
    }
  }

  private def compareInOrder(black: List[Card], white: List[Card]): GameResult = (black, white) match {
    case (Nil, Nil) => Tie
    case (b :: bt, w :: wt) =>
      if (b < w) {
        White
      } else if (b > w) {
        Black
      } else {
        compareInOrder(bt, wt)
      }
  }

  private def comparePairs(black: List[Card], white: List[Card]) = {
    val (bpair, bnpair) = getPairs(black)
    val (wpair, wnpair) = getPairs(white)

    if (bpair.head < wpair.head) {
      White
    } else if (bpair.head > wpair.head) {
      Black
    } else {
      compareInOrder(bnpair.sorted.reverse, wnpair.sorted.reverse)
    }
  }

  private def compareTwoPair(black: List[Card], white: List[Card]) = {
    val (bp1, bp2, brs) = getTwoPairs(black)
    val (wp1, wp2, wrs) = getTwoPairs(white)

    if (bp1.head < wp1.head) {
      White
    } else if (bp1.head > wp1.head) {
      Black
    } else if (bp2.head < wp2.head) {
      White
    } else if (bp2.head > wp2.head) {
      Black
    } else if (brs.head < wrs.head) {
      White
    } else if (brs.head > wrs.head) {
      Black
    } else {
      Tie
    }
  }

  private def compareThreeOfAKind(black: List[Card], white: List[Card]) = {
    val bt = getThree(black)
    val wt = getThree(white)

    if (bt.head < wt.head) White else if (bt.head > wt.head) Black else Tie
  }

  private def compareFourOfAKind(black: List[Card], white: List[Card]) = {
    val bt = getFour(black)
    val wt = getFour(white)

    if (bt.head < wt.head) White else if (bt.head > wt.head) Black else Tie
  }

  private def compareStraightFlush(black: List[Card], white: List[Card]) =
      if (black.sorted.reverse.head < white.sorted.reverse.head){
        White
      } else if (black.sorted.reverse.head > white.sorted.reverse.head) {
        Black
      } else {
        Tie
      }

  private def getPairs(cards: List[Card]): (List[Card], List[Card]) = {
    val p = cards.groupBy(_.value).filter(_._2.length == 2).head._2

    val np = cards.groupBy(_.value).filter(_._2.length != 2).flatMap(_._2)

    (p, np.toList)
  }

  private def getTwoPairs(cards: List[Card]) = {
    val ps = cards.groupBy(_.value).filter(_._2.length == 2)
    val p1 = ps.head._2
    val p2 = ps.tail.head._2
    val rs = cards.groupBy(_.value).filter(_._2.length != 2).head._2

    if (p1.head < p2.head) {
      (p2, p1, rs)
    } else {
      (p1, p2, rs)
    }
  }

  private def getThree(cards: List[Card]) = cards.groupBy(_.value).filter(_._2.length == 3).head._2

  private def getFour(cards: List[Card]) = cards.groupBy(_.value).filter(_._2.length == 4).head._2
}

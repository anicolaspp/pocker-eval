/**
  * Created by anicolaspp on 1/7/17.
  */

sealed trait HandType
case object HighCard extends HandType
case object Pair extends HandType
case object TwoPairs extends HandType
case object ThreeOfAKind extends HandType
case object Straight extends HandType
case object Flush extends HandType
case object FullHouse extends HandType
case object FourOfAKind extends HandType
case object StraightFlush extends HandType


object HandType {

  implicit def toInt(handType: HandType): Int = handType match {
    case HighCard       =>  1
    case Pair           =>  2
    case TwoPairs       =>  3
    case ThreeOfAKind   =>  4
    case Straight       =>  5
    case Flush          =>  6
    case FullHouse      =>  7
    case FourOfAKind    =>  8
    case StraightFlush  =>  9
  }
}
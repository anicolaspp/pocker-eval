/**
  * Created by anicolaspp on 1/6/17.
  */

import org.scalatest.{FlatSpec, Matchers}

import scalaz.{-\/, \/-}

class ParserSpecs extends FlatSpec with Matchers {

  it should "return Left if empty input" in {
    Parser.parseInput("") should be (-\/(EmptyInput))
  }

  it should "return Left if there are not two players" in {
    Parser.parseInput("White: asdasdf") should be (-\/(MissingPlayer))
    Parser.parseInput("Black: 1 2 3 4 5 ") should be (-\/(MissingPlayer))
  }

  it should "return Left if invalid card" in {
    Parser.parseInput("Black: 2H 3D 5S 9C KD White: 2M 3H 4S 8C AH") should be (-\/(InvalidHand))
  }

  it should "parse a card" in {
    val input = "2H"

    val card = Parser.parseCard(input)

    card should be (\/-(H(2)))

  }

  it should "parse any suit" in {
    Parser.parseCard("2C") should be (\/-(C(2)))
    Parser.parseCard("2D") should be (\/-(D(2)))
    Parser.parseCard("2H") should be (\/-(H(2)))
    Parser.parseCard("2S") should be (\/-(S(2)))
  }

  it should "parse any values" in {
    (2 to 9).foreach { v =>
      Parser.parseCard(s"${v}H") should be (\/-(H(v)))
    }

    Parser.parseCard("TH") should be (\/-(H(10)))
    Parser.parseCard("QH") should be (\/-(H(11)))
    Parser.parseCard("KH") should be (\/-(H(12)))
    Parser.parseCard("AH") should be (\/-(H(13)))

    Parser.parseCard("1H") should be (-\/(InvalidCard("1")))
  }

  it should "parse a hand" in {
    val hand = Parser.parseHand("2H 3D 5S 9C KD")

    hand.foreach(_.cards should contain only (H(2), D(3), S(5), C(9), D(12)))
  }

  it should "parse complete input" in {
    val input = "Black: 2H 3D 5S 9C KD White: 2C 3H 4S 8C AH"

    val either = Parser.parseInput(input)

    either.foreach { case (black, white) =>
      black.cards should contain only (H(2), D(3), S(5), C(9), D(12))
      white.cards should contain only (C(2), H(3), S(4), C(8), H(13))
    }
  }
}

class PairEvaluatorSpecs extends FlatSpec with Matchers {
  it should "return unknown is there is not pairs" in {
    val hand = Hand(H(1), H(2), D(3), D(4), C(6))

    PairEvaluator.eval(hand) should be (HighCard)
  }

  it should "return Pair is there is at least a pair" in {
    val hand = Hand(H(2), H(2), D(3), D(4), C(6))

    PairEvaluator.eval(hand) should be (Pair)
  }
}

class TwoPairsEvaluatorSpecs extends FlatSpec with Matchers {
  it should "return unknown is there is not pairs" in {
    val hand = Hand(H(1), H(2), D(3), D(4), C(6))

    TwoPairsEvaluator.eval(hand) should be (HighCard)
  }

  it should "return Pair is there is at least two pairs" in {
    val hand = Hand(H(2), H(2), D(3), D(3), C(6))

    TwoPairsEvaluator.eval(hand) should be (TwoPairs)
  }
}

class ThreeOfAKindEvaluatorSpecs extends FlatSpec with Matchers {
  it should "return unknown is there is 3 cards with same value" in {
    val hand = Hand(H(1), H(2), D(3), D(4), C(6))

    ThreeOfAKindEvaluator.eval(hand) should be (HighCard)
  }

  it should "return ThreeOfAKind is there is at least 3 card with same value" in {
    val hand = Hand(H(2), H(2), D(2), D(3), C(6))

    ThreeOfAKindEvaluator.eval(hand) should be (ThreeOfAKind)
  }
}

class StraightEvaluatorSpecs extends FlatSpec with Matchers {
  it should "return unknown is the values are not consecutive" in {
    val hand = Hand(H(2), H(2), D(2), D(3), C(6))

    StraightEvaluator.eval(hand) should be (HighCard)
  }

  it should "return Straight is the values are consecutive" in {
    val hand = Hand(H(2), H(3), D(4), D(5), C(6))

    StraightEvaluator.eval(hand) should be (Straight)
  }
}

class FlushEvaluatorSpecs extends FlatSpec with Matchers {
  it should "return unknown if the cards have diff suit" in {
    val hand = Hand(H(2), H(2), D(2), D(3), C(6))

    FlushEvaluator.eval(hand) should be (HighCard)
  }

  it should "return Flush if the cards have the same suit" in {
    val hand = Hand(H(2), H(3), H(6), H(10), H(9))

    FlushEvaluator.eval(hand) should be (Flush)
  }
}

class FullHouseEvaluatorSpecs extends FlatSpec with Matchers {
  it should "return unknown if there is not fullhouse" in {
    val hand = Hand(H(2), H(3), H(6), H(10), H(9))

    FullHouseEvaluator.eval(hand) should be (HighCard)
  }

  it should "return FullHouse if there is 3 consecutive value and a pair" in {
    val hand = Hand(C(2), S(2), H(10), H(10), S(10))

    FullHouseEvaluator.eval(hand) should be (FullHouse)
  }
}

class FourOfAKindEvaluatorSpecs extends FlatSpec with Matchers {
  it should "return unknown if not 4 card with same value" in {
    val hand = Hand(H(1), D(1), C(1), S(2), S(3))

    FourOfAKindEvaluator.eval(hand) should be (HighCard)
  }

  it should "return FourOfAKind" in {
    val hand = Hand(H(1), D(1), C(1), S(1), S(3))

    FourOfAKindEvaluator.eval(hand) should be (FourOfAKind)
  }
}

class StraightFlushEvaluatorSpecs extends FlatSpec with Matchers {

  it should "return unknown if no consecutive values" in {
    val hand = Hand(H(1), H(2), D(3), D(4), C(6))

    StraightFlushEvaluator.eval(hand) should be (HighCard)
  }

  it should "return unknown if no card of the same suit" in {
    val hand = Hand(H(1), H(2), D(3), D(4), C(6))

    StraightFlushEvaluator.eval(hand) should be (HighCard)
  }

  it should "return StraightFlush" in {
    val hand = Hand(H(1), H(2), H(3), H(4), H(5))

    StraightFlushEvaluator.eval(hand) should be (StraightFlush)
  }
}

class ChainEvaluatorSpecs extends FlatSpec with Matchers {

  it should "evals hands" in {
    val chain = Chain.full

    chain.eval(Hand(H(1), H(2), D(3), D(4), C(6))) should be(HighCard)

    chain.eval(Hand(H(1), H(2), H(3), H(4), H(5))) should be(StraightFlush)
    chain.eval(Hand(H(1), D(1), C(1), S(1), S(3))) should be(FourOfAKind)
    chain.eval(Hand(C(3), S(3), H(3), D(10), S(10))) should be(FullHouse)
    chain.eval(Hand(H(2), H(3), H(6), H(10), H(9))) should be(Flush)
    chain.eval(Hand(H(2), H(3), D(4), D(5), C(6))) should be(Straight)
    chain.eval(Hand(H(2), H(2), D(2), D(3), C(6))) should be(ThreeOfAKind)
    chain.eval(Hand(H(2), H(2), D(3), D(3), C(6))) should be(TwoPairs)
    chain.eval(Hand(H(2), H(2), D(3), D(4), C(6))) should be(Pair)
  }
}

class HandComparatorSpecs extends FlatSpec with Matchers {

  implicit val chain = Chain.full

  it should "compare equal high card hands" in {

    val hand = Hand(H(1), H(2), D(3), D(4), C(6))

    Hand.compare(hand, hand) should be ((HighCard, Tie))
  }

  it should "compare high card hands" in {
    val black = Hand(H(1), H(2), D(3), D(4), C(6))
    val white = Hand(H(1), H(2), D(3), D(4), C(9))

    Hand.compare(black, white) should be ((HighCard, White))

    val black2 = Hand(H(1), H(2), D(3), D(4), C(10))
    val white2 = Hand(H(1), H(2), D(3), D(4), C(9))
    Hand.compare(black2, white2) should be ((HighCard, Black))
  }

  it should "compare equal pair hands" in {
    val hand = Hand(H(2), H(2), D(3), D(4), C(6))

    Hand.compare(hand, hand) should be ((Pair, Tie))
  }

  it should "compare pair card hands" in {
    val black = Hand(H(2), H(2), D(3), D(4), C(6))
    val white = Hand(H(2), H(2), D(3), D(4), C(9))

    Hand.compare(black, white) should be ((Pair, White))
  }

  it should "compare equal two pair card hands" in {
    val hand = Hand(H(2), H(2), D(3), D(3), C(6))

    Hand.compare(hand, hand) should be ((TwoPairs, Tie))
  }

  it should "compare two pairs card hands" in {
    val black = Hand(H(2), H(2), D(4), D(4), C(6))
    val white = Hand(H(2), H(2), D(4), D(4), C(9))

    Hand.compare(black, white) should be ((TwoPairs, White))
  }

  it should "compare three of a king card hands" in {
    val hand = Hand(H(2), D(2), C(2), D(3), C(6))

    Hand.compare(hand, hand) should be ((ThreeOfAKind, Tie))

    val other = Hand(H(4), D(4), C(4), D(3), C(6))

    Hand.compare(other, hand) should be ((ThreeOfAKind, Black))
  }

  it should "compare straight card hands" in {
    val hand = Hand(H(2), H(3), D(4), D(5), C(6))

    Hand.compare(hand, hand) should be ((Straight, Tie))

    val other = Hand(H(3), H(4), D(5), D(6), C(7))

    Hand.compare(other, hand) should be ((Straight, Black))
  }

  it should "compare flush card hands" in {
    val hand = Hand(H(2), H(3), H(6), H(10), H(9))

    Hand.compare(hand, Hand(C(2), C(3), C(6), C(10), C(9))) should be ((Flush, Tie))

    Hand.compare(hand, Hand(C(2), C(3), C(6), C(10), C(12))) should be ((Flush, White))
  }

  it should "compare full house card hands" in {
    val hand = Hand(C(2), S(2), H(10), H(10), S(10))

    Hand.compare(hand, hand) should be ((FullHouse, Tie))

    Hand.compare(hand, Hand(C(2), S(2), H(11), S(11), D(11))) should be ((FullHouse, White))
  }

  it should "compare four of a kind card hands" in {
    val hand = Hand(H(2), D(2), C(2), S(2), S(3))

    Hand.compare(hand, hand) should be ((FourOfAKind, Tie))

    Hand.compare(hand, Hand(H(3), D(3), C(3), S(3), S(4))) should be ((FourOfAKind, White))
  }

  it should "compare Straight Flush card hands" in {
    val hand = Hand(H(1), H(2), H(3), H(4), H(5))

    Hand.compare(hand, hand) should be ((StraightFlush, Tie))

    Hand.compare(hand, Hand(H(6), H(7), H(8), H(9), H(10))) should be ((StraightFlush, White))
  }
}

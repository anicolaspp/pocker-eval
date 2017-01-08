/**
  * Created by anicolaspp on 1/6/17.
  */

import Hand._

object Parser {

  def parseInput(input: String): (Hand, Hand) = {
    val hands = input.split("White: ")

    val l = hands(0).drop(7)
    val r = hands(1)

    (parseHand(l), parseHand(r))
  }

  def parseHand(s: String): Hand = s.split(" ").map(parseCard)

  def parseCard(card: String): Card = {
    val value = parseValue(card.charAt(0))

    parseSuit(card.charAt(1), withValue = value)
  }

  private def parseValue(char: Char) = char match {
    case '2'  =>  2
    case '3'  =>  3
    case '4'  =>  4
    case '5'  =>  5
    case '6'  =>  6
    case '7'  =>  7
    case '8'  =>  8
    case '9'  =>  9
    case 'T'  =>  10
    case 'Q'  =>  11
    case 'K'  =>  12
    case 'A'  =>  13
  }

  private def parseSuit(char: Char, withValue: Int): Card = char match {
    case 'H'  =>  H(withValue)
    case 'C'  =>  C(withValue)
    case 'D'  =>  D(withValue)
    case 'S'  =>  S(withValue)
  }
}
















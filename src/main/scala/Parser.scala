/**
  * Created by anicolaspp on 1/6/17.
  */

import Hand._

import scalaz.{-\/, \/, \/-}

object Parser {

  def parseInput(input: String): Error \/ (Hand, Hand) = {
    if (input.isEmpty) {
      -\/(EmptyInput)
    } else {

      val hands = input.split("White: ")

      if (hands.length != 2 || !hands(0).startsWith("Black: ")) {
        -\/(MissingPlayer)
      } else {

        val l = hands(0).drop(7)
        val r = hands(1)

        for {
          black <- parseHand(l)
          white <- parseHand(r)
        } yield (black, white)
      }
    }
  }

  def parseHand(s: String): Error \/ Hand = {
    val hand = s.split(" ").map(parseCard)

    if (hand.count(_.isRight) == 5) {
      val cards = hand.map(_.getOrElse(H(2)))
      \/-(cards)
    } else {
      -\/(InvalidHand)
    }
  }

  def parseCard(card: String): Error \/ Card = {
    val value = parseValue(card.charAt(0))

    parseSuit(card.charAt(1), withValue = value)
        .fold[Error \/ Card](e => -\/(InvalidCard(value + e.s)), r => \/-(r))
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

  private def parseSuit(char: Char, withValue: Int): InvalidCard \/ Card = char match {
    case 'H'  =>  \/-(H(withValue))
    case 'C'  =>  \/-(C(withValue))
    case 'D'  =>  \/-(D(withValue))
    case 'S'  =>  \/-(S(withValue))
    case x    =>  -\/(InvalidCard(x.toString))
  }
}

sealed trait Error
case object EmptyInput extends Error
case object MissingPlayer extends Error
case class InvalidCard(s: String) extends Error
case object InvalidHand extends Error

















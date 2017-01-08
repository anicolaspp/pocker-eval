/**
  * Created by anicolaspp on 1/7/17.
  */
sealed trait Card {
  val value: Int
}

case class H(value: Int) extends Card
case class C(value: Int) extends Card
case class D(value: Int) extends Card
case class S(value: Int) extends Card

object Card {
  implicit def order: Ordering[Card] = (x: Card, y: Card) => x.value.compare(y.value)

  implicit def toInt(card: Card): Int = card.value
}
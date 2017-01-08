/**
  * Created by anicolaspp on 1/7/17.
  */
sealed trait Step {
  val next: Step

  def eval(hand: Hand): HandType
}

case class StraightFlushStep(next: Step) extends Step {
  override def eval(hand: Hand): HandType = StraightFlushEvaluator.eval(hand)
}
case class FourOfAKindStep(next: Step) extends Step {
  override def eval(hand: Hand): HandType = FourOfAKindEvaluator.eval(hand)
}
case class FullHouseStep(next: Step) extends Step {
  override def eval(hand: Hand): HandType = FullHouseEvaluator.eval(hand)
}

case class FlushStep(next: Step) extends Step {
  override def eval(hand: Hand): HandType = FlushEvaluator.eval(hand)
}
case class StraightStep(next: Step) extends Step {
  override def eval(hand: Hand): HandType = StraightEvaluator.eval(hand)
}
case class ThreeOfAKindStep(next: Step) extends Step {
  override def eval(hand: Hand): HandType = ThreeOfAKindEvaluator.eval(hand)
}
case class TwoPairsStep(next: Step) extends Step {
  override def eval(hand: Hand): HandType = TwoPairsEvaluator.eval(hand)
}
case class PairStep(next: Step) extends Step {
  override def eval(hand: Hand): HandType = PairEvaluator.eval(hand)
}
case object LastStep extends Step {
  override val next: Step = null

  override def eval(hand: Hand): HandType = HighCard
}

/**
  * Created by anicolaspp on 1/7/17.
  */
class Chain(start: Step) {
  def eval(hand: Hand): HandType = (start.eval(hand), start.next) match {
    case (HighCard, LastStep)  =>  HighCard
    case (HighCard, next)      =>  Chain(next).eval(hand)
    case (value, _)            =>  value
  }
}

object Chain {
  def apply(start: Step) = new Chain(start)

  def full: Chain = Chain(
    StraightFlushStep(
      FourOfAKindStep(
        FullHouseStep(
          FlushStep(
            StraightStep(
              ThreeOfAKindStep(
                TwoPairsStep(
                  PairStep(
                    LastStep
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
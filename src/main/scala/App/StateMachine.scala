package App

import State.State

enum Input {
  case Coin extends Input
  case Turn extends Input
}

case class Machine(locked: Boolean, candies: Int, coins: Int)

val candyMachine = Machine(true, 5, 10)

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
  def updateAndGet(updated: Machine => Machine): State[Machine, Machine] =
    State.modify[Machine](updated).flatMap(_ => State.get[Machine])

  def loop(remaining: List[Input])(
      state: State[Machine, Machine]
  ): State[Machine, Machine] = state.flatMap(s =>
    if s.candies <= 0 then state
    else
      remaining match
        case Nil => state
        case Input.Coin :: t =>
          loop(t)(
            updateAndGet(_ =>
              Machine(
                false,
                s.candies,
                s.coins +
                  1
              )
            )
          )
        case Input.Turn :: t =>
          loop(t)(updateAndGet(_ => Machine(true, s.candies - 1, s.coins)))
        case _ => state
  )

  loop(inputs)(State.get[Machine]).map(s => s.candies -> s.coins)
}

object StateMachine {
  def main(args: Array[String]) = {
    val endMachine = simulateMachine(
      List(
        Input.Coin,
        Input.Turn,
        Input.Coin,
        Input.Turn,
        Input.Coin,
        Input.Turn,
        Input.Coin,
        Input.Turn
      )
    )
    println(endMachine.run(candyMachine))
  }
}

package State

trait State[S, A] {
  def run(s: S): (A, S)

  def flatMap[B](f: A => State[S, B]): State[S, B] = s => {
    val (a, newS) = run(s)
    f(a).run(newS)
  }

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = for {
    a <- this
    b <- rb
  } yield f(a, b)

  def both[B](rb: State[S, B]): State[S, (A, B)] =
    map2(rb)(_ -> _)
}

object State {
  def unit[S, A](a: => A): State[S, A] = (a, _)

  def get[S]: State[S, S] = s => (s, s)

  def modify[S](f: S => S): State[S, Unit] = s => ((), f(s))

  def sequence[S, A](la: List[State[S, A]]): State[S, List[A]] =
    la match
      case Nil    => (List.empty[A], _)
      case h :: t => h.map2(sequence(t))(_ :: _)

}

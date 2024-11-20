package DataStructures

enum Tree[+A] {
  case Leaf(a: A) extends Tree[A]
  case Node(left: Tree[A], right: Tree[A]) extends Tree[A]

  def size: Int =
    fold(_ => 1)(1 + _ + _)

  def maximum[AA >: A](using ev: Ordering[AA]): AA =
    fold(identity)(ev.max)

  def depth: Int =
    fold(_ => 0)(1 + Math.max(_, _))

  def map[B](f: A => B): Tree[B] =
    fold(x => Leaf(f(x)))(Node(_, _))

  def fold[B](f: A => B)(g: (B, B) => B): B =
    this match
      case Leaf(x)    => f(x)
      case Node(l, r) => g(l.fold(f)(g), r.fold(f)(g))

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    this match
      case Leaf(a)           => f(z, a)
      case Node(left, right) => right.foldLeft(left.foldLeft(z)(f))(f)
}

package Parallelism

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

case class Par[A](run: ExecutorService => Future[A]) {
  def map2[B, C](other: Par[B])(f: (A, B) => C): Par[C] =
    Par(es => {
      val futA = run(es)
      val futB = other.run(es)
      new Future[C]:
        override def cancel(mayInterruptIfRunning: Boolean): Boolean = futA
          .cancel(mayInterruptIfRunning) || futB.cancel(mayInterruptIfRunning)

        override def isCancelled: Boolean = futA.isCancelled || futB.isCancelled

        override def isDone: Boolean = futA.isDone && futB.isDone

        override def get(): C = f(futA.get(), futB.get())

        override def get(timeout: Long, unit: TimeUnit): C = {
          val now = System.currentTimeMillis()
          val a = futA.get(timeout, unit)
          val after = System.currentTimeMillis()
          val elapsed = after - now
          val b = futB.get(elapsed, TimeUnit.MILLISECONDS)
          f(a, b)
        }
    })
}

object Par {
  private class UnitFuture[A](a: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def isDone: Boolean = true

    override def get(): A = a

    override def get(timeout: Long, unit: TimeUnit): A = a
  }

  def unit[A](a: A): Par[A] = Par(_ => UnitFuture[A](a))

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def fork[A](a: => Par[A]): Par[A] =
    Par(es => es.submit(() => a.run(es).get()))

  def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a.run(es)
}

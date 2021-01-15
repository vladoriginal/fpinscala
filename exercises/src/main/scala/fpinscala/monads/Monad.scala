package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = lma match {
    case Nil => unit(List.empty[A])
    case h::t => flatMap(h)(a => map(sequence(t))(la => a::la))
  }

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] = la match {
    case Nil => unit(List.empty[B])
    case h::t => map2(f(h), traverse(t)(f))(_::_)
      //                                    ||||
  }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = if (n <= 0) unit(List.empty[A]) else map2(ma, replicateM(n - 1, ma))(_::_)

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(b => g(b))
  def compose_[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = compose[M[A],A,B](identity, f)(ma)

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
}

//noinspection TypeAnnotation
object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  def parMonad: Monad[Par] = ???

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  def streamMonad: Monad[Stream] = ???

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = a :: Nil
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  def stateMonad[S] = {
    type SM[A] = State[S, A]
    new Monad[SM] {
      override def unit[A](a: => A): State[S, A] = State(s => (a, s))
      override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma flatMap f
    }
  }
  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = (f(ma.value))
  }

  def readerMonad[R] = ???
}

case class Id[A](value: A)

case class Reader[R, A](run: R => A) {
  def flatMap[B](f: A => Reader[R,B]): Reader[R,B] = Reader(r => f(run(r)).run(r))
//  def mapR[R1](f: R => R1): Reader[R1,A] = Reader(r => run())
  def getR: Reader[R, R] = Reader(identity)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = st flatMap f
  }

  def t(): Unit = {
//    val f = (r: String) => r.toInt
//    val ma = Reader(f)
    val mma = Reader[String, Reader[String, Int]](r => Reader(r2 => (r + r2).toInt))
    val j = readerMonad.join(mma)
    println(mma.run("1").run("2"))
    println(j.run("3"))
  }
}

object TestMonad extends App {
  Reader.t()
}

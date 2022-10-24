package me.chuwy.otusbats

trait Monad[F[_]] extends Functor[F] { self =>
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def point[A](a: A): F[A]
  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(f => f)
}

object Monad {
  implicit def listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    override def point[A](a: A): List[A] = List(a)
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  implicit def optionMonad: Monad[Option] = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    override def point[A](a: A): Option[A] = Option(a)
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  type EitherType[A] = Either[Any, A]
  implicit def eitherMonad: Monad[EitherType] = new Monad[EitherType] {
    override def flatMap[A, B](fa: EitherType[A])(f: A => EitherType[B]): EitherType[B] = fa.flatMap(f)
    override def point[A](a: A): EitherType[A] = Right(a)
    override def map[A, B](fa: EitherType[A])(f: A => B): EitherType[B] = fa.map(f)
  }
}

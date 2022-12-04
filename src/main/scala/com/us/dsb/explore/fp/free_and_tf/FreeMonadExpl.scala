package com.us.dsb.explore.fp.free_and_tf



// https://www.youtube.com/watch?v=lzlCjgRWPDU
object FreeMonadExpl extends App {
  trait Monad[M[_]] {
    def flatMap[A, B](ma: M[A])(famb: A => M[B]): M[B]
    def pure[A](a: A): M[A] = ???
  }
//  object Monad {
//    def pure[M[_], A](a: A): M[A] = ???
//  }

  trait NatTrans[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  trait Free[M[_], A] {
    import Free.pure
    def flatMap[B](fafmb: A => Free[M, B]): Free[M, B]
    def map[B](fab: A => B): Free[M, B] = flatMap(a => pure(fab(a)))
    def foldMap[G[_]: Monad](natTrans: M NatTrans G): G[A]
  }
  object Free {
    def pure[M[_], A](a: A): Free[M, A] = ???
    def liftM[M[_], A](ma: M[A]): Free[M, A] = ???
  }

  sealed trait MapOps[A]
  case class Set[A](key: String, value: A) extends MapOps[Unit]
  case class Get[A](key: String)           extends MapOps[A]

  type MapFreeMonad[A] = Free[MapOps, A]
  def set[A](key: String, value: A): MapFreeMonad[Unit] =
    Free.liftM[MapOps, Unit](Set(key, value))
  def get[A](key: String): MapFreeMonad[A] =
    Free.liftM[MapOps, A](Get(key))

  def mapComputation: MapFreeMonad[Int/*??*/] = {
    for {
      _ <- set[Int]("key1", 111)
      retrieved <- get[Int]("key1")
    } yield retrieved
  }

  case class IO[A](unsafeRun: () => A)
  object IO {
    def create[A](a: => A): IO[A] = IO(() => a)
  }
  implicit val ioMonad: Monad[IO] = new Monad[IO] {
    override def pure[A](a: A) = IO(() => a)
    override def flatMap[A, B](ma: IO[A])(famb: A => IO[B]): IO[B] =
      IO(() => famb(ma.unsafeRun()).unsafeRun())
  }

  //??? add map starage
  //??? add codec (A vs. given type

  def transformOpsToIO: MapOps NatTrans IO = {
    new (MapOps NatTrans IO) {
      override def apply[A](ma: MapOps[A]): IO[A] = {
        ma match {
          case x @ Set(key @ _ , value @ _) =>
            IO.create {
              println("Simulating impl. of " + x)
              //??? read value
              ()
            }
          case x @ Get(key @ _) =>
            IO.create {
              println("Simulating impl. of " + x)
              //??? save value
              123.asInstanceOf[A]  //??? move hack out
            }
        }
      }
    }
  }
  val ioComputation: IO[Int] = mapComputation.foldMap(transformOpsToIO)

  ioComputation.unsafeRun()

  println("Run something")
}

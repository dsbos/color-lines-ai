

class Temp {

  class A {
    class B
  }

  val x: A = ???;
  //val x2: A.B = ???;
  val x3: A#B = ???;

  import cats.Show
  import cats.instances.int.catsStdShowForInt
  Show.apply.show(123)

  import cats.syntax.show.toShow

  123.show

  //def x: Show

}


package explore.typeclasses


object TypeClassExpl1 extends App {

  trait Doer[T] {
    def getType: String;
    def getTypeView(in: T): String /*= {
      print( getType )
    }*/
  }

  implicit val doerForInt: Doer[Int] = new Doer[Int] {
    //println("Instantiating Doer[Int].");
    override def getType: String = "Int"
    override def getTypeView(in: Int): String = "Int"
  }
  implicit val doerForDouble: Doer[Double] = new Doer[Double] {
    //println("Instantiating Doer[Double].");
    override def getType: String = "Double"
    override def getTypeView(in: Double): String = "Double"
  }
  implicit val doerForChar: Doer[Char] = new Doer[Char] {
    //println("Instantiating Doer[Char].");
    override def getType: String = "Char"
    override def getTypeView(in: Char): String = "Char"
  }

  implicit def doerForList[A](implicit doer: Doer[A]): Doer[List[A]] = new Doer[List[A]] {
    //println("Instantiating Doer[List[A]].");
    override def getType: String = "List[" + doer.getType + "]";
    override def getTypeView(in: List[A]): String = {
      "List[" + doer.getType + "](" + in.map(x => doer.getTypeView(x)).mkString(", ") + ")"
    }
  }
  implicit def doerForColonColon[A](implicit doer: Doer[A]): Doer[::[A]] = new Doer[::[A]] {
    //println("Instantiating Doer[::[A]].");
    override def getType: String = "::[" + doer.getType + "]";
    override def getTypeView(in: ::[A]): String = {
      "::[" + doer.getType + "]" + "(" + in.map(x => doer.getTypeView(x)).mkString(", ") + ")"
    }
  }

  implicit def doerForTuple2[A: Doer, B: Doer]: Doer[(A, B)] = new Doer[(A, B)] {
    //println("Instantiating Doer[(A, B)].");
    def getType: String = "( " + implicitly[Doer[A]].getType + ", " + implicitly[Doer[B]].getType + " )";
    def getTypeView(in: (A, B)): String =
      "(" + implicitly[Doer[A]].getTypeView(in._1) + ", " + implicitly[Doer[B]].getTypeView(in._2) + ")";
  }

  implicit val doerForAny: Doer[Any] = new Doer[Any] {
    //println("Instantiating Doer[An]y.");
    override def getType: String = "Any"
    override def getTypeView(in: Any): String = "Any"
  }
  implicit val doerForNil: Doer[Nil.type] = new Doer[Nil.type] {
    //println("Instantiating Doer[Nil.type].");
    override def getType: String = "Nil.type"
    override def getTypeView(in: Nil.type): String = "Nil.type"
  }


  def getTypeViewBa[A](in2: A)(implicit doer: Doer[A]): String = {
    doer.getTypeView(in2)
  }
  def getTypeViewBb[A: Doer](in2: A): String = {
    implicitly[Doer[A]].getTypeView(in2)
  }
  def getTypeView2[A: Doer](in2: A): String = {
    implicitly[Doer[A]].getTypeView(in2)
  }

/*
  implicit class xxx[A](in: A) {
    def doSomething2(implicit doer: Doer[A]): Unit = {
      doer.doSomething2(in)
    }
  }
*/
  println("------")
  println(getTypeViewBa(3))
  println(getTypeViewBb(3))
  println(getTypeView2(3))
  //3.doSomething2
  println("------")
  println(getTypeViewBa(1.1))
  println(getTypeViewBb(2.2))
  println(getTypeView2(3.3))
  println("------")
  println(getTypeView2(3 :: Nil))
  println(getTypeView2(1 :: 2 :: Nil))
  //doSomethingBa(3 :: "three" :: Nil)
  //println()
  //doSomething2(3 :: "three" :: Nil)
  //println()
  println("------")
  println(getTypeView2((3, 3.1)))
  println(getTypeView2((3, 3.1)))
  println("------")
  println(getTypeView2( List( List(1), List(2) )))
  println(getTypeView2( List( List(1), 2 )))
  println(getTypeView2( List( List(1), List(2).asInstanceOf[::[Int]] )))
  println(getTypeView2( List(2).asInstanceOf[::[Int]] ))
  println(getTypeView2((3,  ( ( 3.1, Nil ), 'x') )))
  println(getTypeView2((3,  ( ( 3.1, List[Int]() ), 'x') )))


  //val x: String  = '1' :: 1.2d :: 'x' :: Nil


  import cats.Monoid
  import cats.instances.int._
  println(Monoid[Int].combine(3, 3))



}


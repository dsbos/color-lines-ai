package strongtypes

// https://github.com/estatico/scala-newtype
import io.estatico.newtype.macros._


object CustomConstructorNewtypes extends App {
  // Quiet warning "implicit conversion method opsThis should be enabled":
  import scala.language.implicitConversions

  // (Newtypes must be in an object, since macro defines a type.)

  @newtype case class Plain(raw: Int)

  @newtype /*case*/ class SuppressedConstructor(raw: Int)
  object SuppressedConstructor {
    def construct(x: Float): SuppressedConstructor = {
      import io.estatico.newtype.ops._  // for .coerce
      x.toInt.coerce
    }
    def apply(one: Int, other: Int): SuppressedConstructor = {
      import io.estatico.newtype.ops._  // for .coerce
      (one - other).toInt.coerce
    }
  }

  Plain(0)
  //Plain()
  Plain
  //new Plain(0)
  //new Plain()
  //new Plain

  //SuppressedConstructor(0)
  //SuppressedConstructor()
  SuppressedConstructor

  SuppressedConstructor.construct(0)
  //SuppressedConstructor(0)
  //SuppressedConstructor()
  SuppressedConstructor
  //new SuppressedConstructor(0)
  //new SuppressedConstructor()
  //new SuppressedConstructor

  SuppressedConstructor(1, 2)
}

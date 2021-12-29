package namethis

/*
GET /brands
POST /brands
GET /categories
POST /categories
GET /items
GET /items?brand=gibson
POST /items
PUT /items
GET /cart
POST /cart
PUT /cart
DELETE /cart/{itemId}
GET /orders
GET /orders/{orderId}
POST /checkout
POST /auth/users
POST /auth/login
POST /auth/logout”
*/


package unclear_refs {
  case class UUID()  // no import specified - whose UUID class?
  case class Card()  // seems to be missing from chapter 4
}
import unclear_refs._

//import scala.language.higherKinds


//import eu.timepit.refined.types.string.NonEmptyString

//import io.estatico.newtype.macros.newtype


case class User(id: UserId, name: UserName)


/*@newtype*/ case class BrandName(value: String)
/*@newtype*/ case class BrandId(value: UUID)

case class Brand(uuid: BrandId, name: BrandName)

trait Brands[F[_]] {
  def findAll: F[List[Brand]]
  def create(name: BrandName): F[Unit]
}


/*@newtype*/ case class CategoryId(value: UUID)
/*@newtype*/ case class CategoryName(value: String)
case class Category(uuid: CategoryId, name: CategoryName)

trait Categories[F[_]] {
  def findAll: F[List[Category]]
  def create(name: CategoryName): F[Unit]
}


/*@newtype*/ case class ItemId(value: UUID)
/*@newtype*/ case class ItemName(value: String)
/*@newtype*/ case class ItemDescription(value: String)

import squants.market.Money
case class Item(
    uuid: ItemId,
    name: ItemName,
    description: ItemDescription,
    price: Money,
    brand: Brand,
    category: Category
)

case class CreateItem(
    name: ItemName,
    description: ItemDescription,
    price: Money,
    brandId: BrandId,
    categoryId: CategoryId
)

case class UpdateItem(
    id: ItemId,
    price: Money
)

trait Items[F[_]] {
  def findAll: F[List[Item]]
  def findBy(brand: BrandName): F[List[Item]]
  def findById(itemId: ItemId): F[Option[Item]]
  def create(item: CreateItem): F[Unit]
  def update(item: UpdateItem): F[Unit]
}

/*@newtype*/ case class UserId(value: UUID)



/*@newtype*/ case class CartId(value: UUID)
/*@newtype*/ case class Quantity(value: Int)
/*@newtype*/ case class Cart(items: Map[ItemId, Quantity])
case class CartItem(item: Item, quantity: Quantity)
case class CartTotal(items: List[CartItem], total: Money)

trait ShoppingCart[F[_]] {
  def add(
    userId: UserId,
    itemId: ItemId,
    quantity: Quantity
  ): F[Unit]
  def delete(userId: UserId): F[Unit]
  def get(userId: UserId): F[CartTotal]
  def removeItem(userId: UserId, itemId: ItemId): F[Unit]
  def update(userId: UserId, cart: Cart): F[Unit]
}



/*@newtype*/ case class OrderId(uuid: UUID)

/*@newtype*/ case class PaymentId(uuid: UUID)

case class Order(
  id: OrderId,
  pid: PaymentId,
  items: Map[ItemId, Quantity],
  total: Money
)

trait Orders[F[_]] {
  def get(
    userId: UserId,
    orderId: OrderId
  ): F[Option[Order]]

  def findBy(userId: UserId): F[List[Order]]

  def create(
    userId: UserId,
    paymentId: PaymentId,
    items: List[CartItem],
    total: Money
  ): F[OrderId]
}


/*@newtype*/ case class UserName(value: String)
/*@newtype*/ case class Password(value: String)

trait Users[F[_]] {
  def find(
    username: UserName,
    password: Password
  ): F[Option[User]]

  def create(
    username: UserName,
    password: Password
  ): F[UserId]
}




/*@newtype*/ case class JwtToken(value: String)

trait Auth[F[_]] {
  def findUser(token: JwtToken): F[Option[User]]
  def newUser(username: UserName, password: Password): F[JwtToken]
  def login(username: UserName, password: Password): F[JwtToken]
  def logout(token: JwtToken, username: UserName): F[Unit]
}


case class Payment(
    id: UserId,
    total: Money,
    card: Card
)

trait PaymentClient[F[_]] {
  def process(payment: Payment): F[PaymentId]
}




import cats.Monad
//import cats.syntax._
import cats.implicits._

//package object dummy {
//  cats.Monad.flatMap -- doesn't exist
//}


import scala.concurrent.duration.FiniteDuration
trait Background[F[_]] {
   def schedule[A](
       fa: F[A],
       duration: FiniteDuration
   ): F[Unit]
}


final class CheckoutProgram[F[_]: Monad](
    paymentClient: PaymentClient[F],
    shoppingCart: ShoppingCart[F],
    orders: Orders[F]
) {

/*
  implicit def concurrentBackground[
      F[_]: Concurrent: Timer
  ]: Background[F] =
    new Background[F] {

      def schedule[A](
          fa: F[A],
          duration: FiniteDuration
      ): F[Unit] =
        (Timer[F].sleep(duration) *> fa).start.void

    }
*/



  def checkout(userId: UserId, card: Card): F[OrderId] =
    for {
      // get "value flatMap is not a member of type parameter F[namethis.CartTotal]"
      cart <- shoppingCart.get(userId)

      paymentId <- paymentClient.process(
        Payment(userId, cart.total, card)
        )
      orderId <- orders.create(
        userId, paymentId, cart.items, cart.total
        )
      _ <- shoppingCart.delete(userId)  // ???? .attempt.void
    } yield orderId


  /*
  import cats.effect.Timer
  import scala.concurrent.duration.DurationConversions._

  def retry[A](fa: F[A]): F[A] =
     Timer[F].sleep(50.milliseconds) >> fa
  */

  import retry.RetryPolicies._
  import retry.RetryDetails
  import retry.RetryDetails.WillDelayAndRetry
  import retry.RetryDetails.GivingUp

/*
  def logError(
      action: String
  )(
      e: Throwable,
      details: RetryDetails
  ): F[Unit] =
    details match {
      case r: WillDelayAndRetry =>
        Logger[F].error(
          s"Failed on $action. We retried ${r.retriesSoFar} times."
        )
      case g: GivingUp =>
        Logger[F].error(
          s"Giving up on $action after ${g.totalRetries} retries."
        )
    }

  val retryPolicy =
    limitRetries[F](3) |+| exponentialBackoff[F](10.milliseconds)

  def processPayment(payment: Payment): F[PaymentId] = {
    val action = retryingOnAllErrors[PaymentId](
      policy = retryPolicy,
      onError = logError("Payments")
    )(paymentClient.process(payment))

    action.adaptError {
      case e =>
        PaymentError(
          Option(e.getMessage).getOrElse("Unknown")
        )
    }
  }

  def createOrder(
      userId: UserId,
      paymentId: PaymentId,
      items: List[CartItem],
      total: Money
  ): F[OrderId] = {
    val action = retryingOnAllErrors[OrderId](
      policy = retryPolicy,
      onError = logError("Order")
    )(orders.create(userId, paymentId, items, total))

    def bgAction(fa: F[OrderId]): F[OrderId] =
      fa.adaptError {
          case e => OrderError(e.getMessage)
        }
        .onError {
          case _ =>
            Logger[F].error(
              s"Failed to create order for: ${paymentId}"
            ) *>
              Background[F].schedule(bgAction(fa), 1.hour)
        }

    bgAction(action)
  }
*/


}


/*

This is the simplest implementation with the desired semantics. We could have chosen to do it differently, e.g. using a Queue. We could have also used the native background method provided by Cats Effect, which is a safer alternative to start. However, if we understand its trade-offs, this is more than acceptable.

Finally, let’s stare in awe at our final checkout implementation.

def checkout(userId: UserId, card: Card): F[OrderId] =
  shoppingCart.get(userId)
    .ensure(EmptyCartError)(_.items.nonEmpty)
    .flatMap {
      case CartTotal(items, total) =>
        for {
          pid <- processPayment(Payment(userId, total, card))
          order <- createOrder(userId, pid, items, total)
          _ <- shoppingCart.delete(userId).attempt.void
”

Excerpt From: Gabriel Volpe. “Practical FP in Scala.” Apple Books.

“allows arbitrary side-effects.
We achieve better testability, as we will see in Chapter 7.
For completeness, here is our default Background instance:

implicit def concurrentBackground[
    F[_]: Concurrent: Timer
]: Background[F] =
  new Background[F] {

    def schedule[A](
        fa: F[A],
        duration: FiniteDuration
    ): F[Unit] =
      (Timer[F].sleep(duration) *> fa).start.void

  }
This is the simplest implementation with the desired semantics. We could have chosen to do it differently, e.g. using a Queue. We could have also used the native background method provided by Cats Effect, which is a safer alternative to start. However, if we understand its trade-offs, this is more than acceptable.

Finally, let’s stare in awe at our final checkout implementation.

def checkout(userId: UserId, card: Card): F[OrderId] =
  shoppingCart.get(userId)
    .ensure(EmptyCartError)(_.items.nonEmpty)
    .flatMap {
      case CartTotal(items, total) =>
        for {
          pid <- processPayment(Payment(userId, total, card))
          order <- createOrder(userId, pid, items, total)
          _ <- shoppingCart.delete(userId).attempt.void
        } yield order
    }
It has never been easier to manage effects in a purely functional way in Scala. Composing retry policies using standard typeclasses and sequencing actions using monadic combinators led us to our ultimate solution.

Our final class is defined as follows (MonadThrow is a type alias for MonadError[F, Throwable]):

final class CheckoutProgram[
    F[_]: Background: Logger: MonadThrow: Timer
](
    paymentClient: PaymentClient[F],
    shoppingCart: ShoppingCart[F],
    orders: Orders[F],
    retryPolicy: RetryPolicy[F]
) { ... }







 */
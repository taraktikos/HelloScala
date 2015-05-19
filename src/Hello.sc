import intro.MyModule._
import datastructures._

val x = List(1, 2, 3, 4, 5) match {
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case Cons(h, t) => h + List.sum(t)
  case _ => 101
}
val list = List(1, 2, 3, 4, 5)
//List.tail(list)

List.dropWhile(list, (x: Int) => x > 2)

List.append(List(1,2), List(3,4))
class CreditCard {
  def charge(price: Int): Unit = {

  }
}
class Coffee {
  val price = 2
}
def buyCoffee(cc: CreditCard): Coffee = {
  val cup = new Coffee()
  cc.charge(cup.price)
  cup
}
buyCoffee(new CreditCard)
fib(4000)
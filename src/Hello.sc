import intro.MyModule._

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
val x = "hello"
val r1 = x.reverse
val r2 = x.reverse

2.+(1)

fib(4000)
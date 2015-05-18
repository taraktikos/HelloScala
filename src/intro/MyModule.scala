package intro

object MyModule {
  def abs(n: Int): Int = if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val msg = "Abs %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactoraial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n*acc)
    go(n, 1)
  }

  def fib(n: Int): Int = {
    def go(n: Int, prev: Int, current: Int): Int = {
      val next = prev + current
      print(next + " ")
      if (n > 0) {
        go(n - 1, current, next)
      }
      next
    }
    go(n - 1, 0, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def add(a: Int, b: Int): Int = {
    a + b
  }

  def main(args: Array[String]): Unit = {
    val curryAdd = curry(add)
    println("Curry add " + curryAdd(2)(3))
    println("unCurry add " + uncurry(curryAdd)(2, 3))
    println("Compose " + compose((x: Int) => x - 1, (y: Int) => y + 2)(5))
    println(formatResult("abs", -3, abs))
    println(formatResult("factorial", 4, factorial))
    println(isSorted(Array(7, 9, 14).reverse, (a1: Int, a2: Int) => a1 > a2))
  }
}

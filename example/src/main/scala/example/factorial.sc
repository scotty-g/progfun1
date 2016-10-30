object factorial {
  def factorial1(x: Int): Int = {
    if (x == 0) 1 else x * factorial1(x - 1)
  }

  factorial1(1)
  factorial1(2)
  factorial1(3)
  factorial1(4)
  factorial1(5)

  def factorial2(x: Int): Int = {
    def loop(acc: Int, x: Int): Int = {
      if(x == 0) acc
      else loop(acc * x, x - 1)
    }
    loop(1, x)
  }

  factorial2(1)
  factorial2(2)
  factorial2(3)
  factorial2(4)
  factorial2(5)
}
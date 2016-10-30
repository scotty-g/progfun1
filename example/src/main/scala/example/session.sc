object session {
  1+3
  def abs(x: Double) = if (x < 0) -x else x

  def sqrtIter(guess: Double, x: Double): Double = {
    if(isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  }

  def isGoodEnough(guess: Double, x: Double): Boolean = {
    abs(guess * guess - x) / x < 0.001
  }

  def improve(guess: Double, x: Double): Double = {
    (guess + x / guess) / 2
  }

  def sqrt(x: Double): Double = sqrtIter(1.0, x)

  sqrt(2)
  sqrt(4)
  sqrt(9)
  sqrt(10)
  sqrt(1e-6)
  sqrt(1e60)
}
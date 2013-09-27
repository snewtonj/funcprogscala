class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must be non-zero")


  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  val g = gcd(x, y)

  def numer = x / g
  def denom = y / g

  def this(x: Int) = this(x,1)

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)

  def neg = new Rational(-numer, denom)

  def sub(that: Rational) =
    add(that.neg)

  def mul(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)

  def div(that: Rational) =
    new Rational(numer * that.denom, denom * that.numer)

  override def toString = {
    numer + "/" + denom
  }
}

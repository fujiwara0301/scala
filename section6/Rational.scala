class Rational(n: Int, d: Int) {

  /** 分母は0禁止 */
  require(d != 0)

  /** 最大公約数 */
  private val g = gcd(n.abs, d.abs)

  /** 分子 */
  val numer: Int = n / g

  /** 分母 */
  val denom: Int = d / g
  def this(n: Int) = this(n, 1) // 補助コンストラクター
  override def toString = numer  + "/" + denom + " gcd = " + g
  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
  /** 足し算 */
  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )
  def + (i: Int): Rational =
    new Rational(numer + i * denom, denom)
  def - (that: Rational): Rational =
    new Rational(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )
  def - (i: Int) : Rational =
    new Rational(numer - i * denom, denom)
  def * (that: Rational): Rational =
    new Rational(
      numer * that.numer, denom * that.denom
    )
  def * (i: Int): Rational =
    new Rational(numer * i, denom)
  def / (that: Rational): Rational =
    new Rational(numer * that.denom, denom * that.numer)
  def / (i: Int): Rational =
    new Rational(numer, denom * i)
  def lessThan(that: Rational) =
    this.numer * that.denom < that.numer * this.denom
  def moreThan(that: Rational): Boolean =
    this.numer * that.denom > that.numer * this.denom
  def max(that: Rational) =
    if (this.lessThan(that)) that else this
}

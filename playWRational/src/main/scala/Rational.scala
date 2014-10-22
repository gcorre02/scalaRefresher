/**
 * Created by Guilherme on 21/10/2014.
 */
class Rational(n: Int, d: Int) {
  require(d != 0, "A rational number cannot be divided by zero")
  def getN():Int = n
  def getD():Int = d
  def this(n:Int) = this(n, 1)
  override def toString = n + "/" + d
  def *(p:Rational):Rational = new Rational(n * p.getN(),d * p.getD())
  def /(p:Rational):Rational = new Rational(n * p.getD(),d * p.getN())

  def normalizeAndOPDenominators(left: Rational, right: Rational, op:(Int,Int) => Int) = {
    val newDenominator = left.getD() * right.getD()
    val leftNominator = left.getN() * right.getD()
    val rightNominator = right.getN() * left.getD()
    val finalRational = new Rational(op(leftNominator,rightNominator),newDenominator)
    finalRational
  }


  def +(p:Rational):Rational = normalizeAndOPDenominators(this,p,(a,b) => a + b)
  def -(p:Rational):Rational = normalizeAndOPDenominators(this,p,(a,b) => a - b)
}


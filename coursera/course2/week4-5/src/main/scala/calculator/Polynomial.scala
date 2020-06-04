package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal[Double] {
      //b² - 4ac
    b() * b() - ( 4 * a() * c() )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal[Set[Double]] {
    //(-b ± √Δ) / 2a
    val x = (0 - b()) - computeDelta(a, b, c)() / (2 * a())
    val y = (0 - b()) + computeDelta(a, b, c)() / (2 * a())
    Set[Double](x, y)
  }
}

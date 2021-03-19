import scala.math.abs

class Line(ps:Array[Point]) {

  // read only values a and b
  def a: Double = Util.cov(ps.map(p => p.x), ps.map(p => p.y)) / Util.variance(ps.map(p => p.x))
  def b: Double = (ps.map(p => p.y).sum / ps.length) - a * (ps.map(p => p.x).sum / ps.length)

  // f
  def f(x: Double): Double = {
    a * x + b
  }

  // dist
  def dist(p: Point): Double = {
    abs(f(p.x) - p.y)
  }
}

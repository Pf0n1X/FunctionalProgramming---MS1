import scala.math.{log10, pow, sqrt}

object Util {

  // max
  def max[A](list: List[A], comp: (A, A) => Int): A = {
    list.reduceLeft((a, b) =>
      if (comp(a, b) >= 0) a else b
    )
  }

  // map
  def map[A, B, C](list: List[A], f1: A => B, f2: B => C): List[C] = {
    return list.map(f1).map(f2);
    //    list.map(a => f2(f1(a)));
  }

  // isSorted
  def isSorted[A](list: List[A], comp: (A, A) => Boolean): Boolean = {
    if (list.length == 2) comp(list(0), list(1)) else comp(list(0), list(1)) && isSorted(list.drop(1), comp);
  }

  // probs
  def probs(xs: Array[Double]) = {
    xs.map(el1 => xs.count(el2 => el1.equals(el2)).toDouble / xs.length)
  }

  // entropy
  def entropy(xs: Array[Double]): Double = {
    -probs(xs).map((value) => value * (log10(value) / log10(2))).sum
  }

  // mu
  def mu(xs: Array[Double]): Double = {
    probs(xs).zipWithIndex.map { case (el, index) => el * xs(index) }.sum
  }

  // variance
  def variance(xs: Array[Double]): Double = {
    probs(xs).zipWithIndex.map { case (pi, i) => ((pi * pow((xs(i) - mu(xs)), 2))) }.sum
  }

  // zscore
  def zscore(xs: Array[Double], x: Double): Double = {
    (x - mu(xs)) / sqrt(variance(xs))
  }

  // cov
  def cov(xs: Array[Double], ys: Array[Double]): Double = {
    mu(xs.zipWithIndex.map { case (x, index) => (x - mu(xs)) * (ys(index) - mu(ys)) })
  }

  // pearson
  def pearson(xs: Array[Double], ys: Array[Double]): Double = {
    cov(xs, ys) / (sqrt(variance(xs)) * sqrt(variance(ys)))
  }
}

package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val bb = b()
      bb * bb - (4 * a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      val ds = Math.sqrt(delta())
      if(ds > 0) {
        Set((ds - b()) / (2* a()), (- ds - b()) / (2 * a()))
      }else {
        Set.empty[Double]
      }
    }
  }
}

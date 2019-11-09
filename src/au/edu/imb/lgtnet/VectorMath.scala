package au.edu.imb.lgtnet

import math.{sqrt,abs}

/**
 * Functions mainly for math on vectors.
 */
object VectorMath {
  type V = Seq[Double]

  def mean(vs:V) = vs.sum / vs.length.toDouble
  
  def max(vs:V) = vs.max
    
  def min(vs:V) = vs.min
  
  def std(vs:V) = {
    val m = mean(vs)
    sqrt( vs.map(v => (v-m)*(v-m)).sum / vs.length )
  }
  
  def median(vs:V):Double = {
    val n = vs.length
    if(n==0) return 0.0
    if(n==1) return vs(0)
    val xs = vs.sorted
    if(n % 2 == 0) (xs(n/2-1)+xs(n/2))/2.0 else xs(n/2)
  }

  def diff(xs:V,ys:V) = (xs zip ys) map {case (x,y) => x-y}
  
  def absdiff(xs:V,ys:V) = (xs zip ys) map {case (x,y) => abs(x-y)}
}

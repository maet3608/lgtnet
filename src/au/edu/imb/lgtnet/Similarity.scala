package au.edu.imb.lgtnet

import scala.math._


/**
 * Different method to measure sequence similarity
 */
object Similarity {
  // used for logDet only


  // used for fft only
  private val nuc2vec = Map('A' -> List(1,0,0,0),'C' -> List(0,1,0,0),'T' -> List(0,0,1,0),'G' -> List(0,0,0,1) )

  /** Returns hamming distance scaled to sequence length.
    * Requires equal length sequences */
  def hamming(s1:MolecularSequence, s2:MolecularSequence) =
    (s1 zip s2).count{case (a,b) => a!=b} / s1.length.toDouble

  /** Returns euclidean distance */
  def euclidean(s1:MolecularSequence, s2:MolecularSequence) =
    sqrt(hamming(s1,s2))


  /** Returns ngram distance */
  def ngram(s1:MolecularSequence, s2:MolecularSequence, n:Int) = {
    def ngrams(s:MolecularSequence) = s.letters.sliding(n).toSet
    val ngrams1 = ngrams(s1)
    val ngrams2 = ngrams(s2)
    //(ngrams1 & ngrams2).size.toDouble / min(ngrams1.size,ngrams2.size) // local
    (ngrams1 & ngrams2).size.toDouble / max(ngrams1.size,ngrams2.size)  // global
  }

  /** longest common subsequence
   * http://en.wikipedia.org/wiki/Longest_common_substring_problem
   * */
  def lcs(s1:MolecularSequence, s2:MolecularSequence) = {
    val L = Array.ofDim[Int](s1.length+1,s2.length+1)
    for(i <- 1 to s1.length; j <- 1 to s2.length)
      L(i)(j) = if(s1(i-1)!=s2(j-1)) 0 else L(i-1)(j-1)+1
    (1 to s1.length).map(L(_).max.toDouble).sum
  }

}


/** Usage example */
object SimilarityExample extends App {
  val s1 = MolecularSequence("AAAAAAAAAA", DNA)
  val s2 = MolecularSequence("AAAAAAAACC", DNA)
  println(Similarity.hamming(s1,s2))
}
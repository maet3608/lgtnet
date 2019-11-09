package au.edu.imb.lgtnet

import math._
import Similarity._
import java.io.BufferedWriter
import java.io.FileWriter


/**
 * Predictors of LGT events.
 */
abstract class LGTPredictor {
  type Metric = (Seq[Double],Seq[Boolean]) => Double
  type Pair = (SequenceNode,SequenceNode)

  /**
   * Predicts LGT events
   * @param nodes  List of sequence nodes to predict LGT events between
   * @return Returns list of sequence-node pairs with an event score.
   */
  def predict(nodes:List[SequenceNode]):List[(Pair,Double)]

  /**
   * Computes the performance of the predictor for a given tree and event
   * @param tree Tree with evolved sequences
   * @param event LGT event used with that tree
   * @param metric Performance metric, e.g. Metric.AUC
   * @return Returns the AUC
   */
  def performance(tree:SequenceNode, event:LGTEvent, metric:Metric) = {
    val (pairs,predictions) = predict(tree.leaves).unzip
    val trueEvents = event.leafEvents(tree).toSet
    val targets = pairs.map{case (a,b) => trueEvents.contains(Set(a,b))}
    //for((p,t,pr) <- (pairs,targets,predictions).zipped.toList) printf("%s-%s %s %.3f\n",p._1.name,p._2.name,t,pr)
    metric(predictions, targets)
  }

  /** Returns list with all node pairs (triangular matrix) */
  def pairedNodes(nodes:List[SequenceNode]) =
    (for((ni,i) <- nodes.zipWithIndex; (nj,j) <- nodes.zipWithIndex if i<j) yield (ni,nj))
}

/**
 * A simple predictor that uses a sliding window approach.
 * Distances between the aligned sequences are computed. Then a window slides
 * over the aligned sequences, distances between sequence windows are computed
 * and then the differences between the overall distances and the window based
 * distances are computed. The means over those differences are returned as
 * scores.
 */
class LGTPredictorAlign(method:String, dist:String, w:Int=10) extends LGTPredictor {
  val f = method match {
    case "max" => (v:Seq[Double]) => {VectorMath.max(v)-VectorMath.mean(v)}
    case "min" => (v:Seq[Double]) => {VectorMath.mean(v)-VectorMath.min(v)}
    case "std" => VectorMath.std(_)
  }
  val f_dist = dist match {
    case "hm" => hamming(_,_)
    case "eu" => euclidean(_,_)
    case "lc" => lcs(_,_)
    case ng => ngram(_:MolecularSequence,_:MolecularSequence,ng.drop(2).toInt)
  }

  def predict(nodes:List[SequenceNode]) = {
    val pairs = pairedNodes(nodes)
    val len = nodes.head.sequence.length
    val step = max(1,w/2)  // step size

    def distances(from:Int, until:Int) =
      pairs.map{case (a,b) => f_dist(a.sequence(from,until), b.sequence(from,until))}

    val D = distances(0,len)
    val wD = (0 to (len-step) by step) map (i => distances(i,i+w))
    val wDiffs = wD map (VectorMath.absdiff(D,_))
    val scores = wDiffs.transpose.map(f)

    pairs zip scores
  }

  override def toString = "A-"+method+":"+dist+":"+w
}






class LGTPredictorNGramV1(method:String, n:Int, w:Int) extends LGTPredictor {
  type NGrams = Map[String,Int]

  def ngrams(s:MolecularSequence):NGrams =
    s.letters.sliding(n).zipWithIndex.toMap

  def positions(ngrams1:NGrams, ngrams2:NGrams):List[Int] = {
    def pos(ngrams1:NGrams, ngrams2:NGrams) =
      (ngrams1.keySet & ngrams2.keySet).toList.map(ngrams2)
    pos(ngrams1,ngrams2):::pos(ngrams2,ngrams1)
  }

  def histogram(positions:Iterable[Int], minPos:Int, maxPos:Int):Seq[Double] = {
    val nBins = 1+maxPos/w
    def index(pos:Int) = ((nBins-1.0)*(pos-minPos)/(maxPos-minPos)).toInt
    val bins = Array.fill(nBins)(0.0)
    positions.foreach(p => bins(index(p)) += 1.0)
    bins
  }

  def score(histogram:Seq[Double]) = method match {
    case "max" => VectorMath.max(histogram)-VectorMath.mean(histogram)
    case "std" => VectorMath.std(histogram)
  }

  def predict(nodes:List[SequenceNode]):List[(Pair,Double)] = {
    val pairs = pairedNodes(nodes)
    val node2ngrams = nodes.map(n => (n,ngrams(n.sequence))).toMap
    val pairedNgrams = pairs.map{case (a,b) => (node2ngrams(a),node2ngrams(b))}
    val pos = pairedNgrams.map{case (a,b) => positions(a,b)}
    val posFlat = pos.flatten
    if(posFlat.isEmpty) 
      throw new Exception("No n-gram matches! n-grams too long or window too small/large?")
    val (minPos, maxPos) = if(posFlat.isEmpty) (0,0) else (posFlat.min, posFlat.max)
    val scores = pos.map(p => score(histogram(p,minPos,maxPos)))
    pairs zip scores
  }

  override def toString = "N-"+method+":"+n+":"+w
}





class LGTPredictorNGram(mode:String, n:Int, w:Int, writer:ProfileWriter=null) extends LGTPredictor {
  type NGrams = Map[String,Int]
  
  def ngrams(s:MolecularSequence):NGrams =
    s.letters.sliding(n).zipWithIndex.toMap

  def positions(ngrams1:NGrams, ngrams2:NGrams):List[Int] = {
    val union = (ngrams1.keySet & ngrams2.keySet).toList
    union.map(ngrams1):::union.map(ngrams2)
  }
  
  def mean(bins:Seq[Int]):Double = bins.sum / bins.length.toDouble

  def score(pair:Pair, positions:Iterable[Int], minPos:Int, maxPos:Int):Double = {
    val n = maxPos/w
    val bins = Array.fill(n)(0)
    def index(pos:Int) = ((n-1.0)*(pos-minPos)/(maxPos-minPos)).toInt
    positions.foreach(p => bins(index(p)) += 1)   
    if(writer!=null) writer.write(pair._1.name, pair._2.name, bins)
    val (mx,me) = (bins.max, mean(bins)) 
    if(mx>0) (mx-me)/mx else 0.0
  }
      
  def predict(nodes:List[SequenceNode]):List[(Pair,Double)] = {
    def toNgrams(node:SequenceNode) = ngrams(node.sequence)
    val ns = if(mode=="high") nodes.par else nodes
    val node2ngrams = if(mode=="low") toNgrams _ else ns.map(n => (n,toNgrams(n))).toList.toMap
    def process(pair:Pair) = {
      print(".")
      if(mode=="low") System.gc()
      val pos = positions(node2ngrams(pair._1),node2ngrams(pair._2))
      if(pos.isEmpty) 0.0 else score(pair,pos,pos.min,pos.max)      
    } 
    val pairs = if(mode=="high") pairedNodes(nodes).par else pairedNodes(nodes)
    pairs.map(pair => (pair,process(pair))).toList
  } 
  
  override def toString = "LGTNet-"+mode
}



/**
 * A random LGT predictor as control.
 */
class LGTPredictorControl extends LGTPredictor {
  def predict(nodes:List[SequenceNode]) =
    pairedNodes(nodes) map {p => (p,random)}
  override def toString = "Ctrl"
}

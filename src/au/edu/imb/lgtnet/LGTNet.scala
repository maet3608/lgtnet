package au.edu.imb.lgtnet

import scala.io.Source
import java.io.{BufferedWriter,FileWriter,PrintWriter}
import sys.process.stdout
import java.io.StringWriter
import java.io.File


/** Writes profiles to the given file */
class ProfileWriter(filename:String)  {
  private val writer = new BufferedWriter(new FileWriter(filename))
    
  def write(id1:String, id2:String, bins:Array[Int]) {
    synchronized {
      writer.write(bins.mkString(id1+"-"+id2+"\t", "\t" ,"\n"))
      writer.flush()
    }
  } 
  
  def close() {
    writer.close()
  }
}

/**
 * The main object that loads sequences from a file, infers an LGT network and
 * writes the result to a file in matrix form.
 */
object LGTNet {
  type Pair = (SequenceNode,SequenceNode)
  val STDOUT = "stdout"
    
  def readFasta(filename:String) = {
     val header = """>(.+?)([\s,\|].+)?""".r
     def parse(lines:Iterator[String]):List[(String,String)] = {
       if(lines.isEmpty) return List()
       val header(name,annotation) = lines.next
       val (seq,rest) = lines.span(_(0)!='>')
       (name, seq.mkString)::parse(rest)
     }
     parse(Source.fromFile(filename).getLines().map(_.trim).filterNot(_.isEmpty()))
  }
  
  /** Concerts sequences given as tuples (name, sequence_string) to list of SequenceNode */
  def toSequenceNodes(sequences:List[(String,String)], alphabet:Alphabet) =
    sequences map {case (n,s) => SequenceNode(n, 0.0, MolecularSequence(s, alphabet), List())}
  
  /** Reads sequences from either a single file or a folder with multiple FASTA files. */
  def readSequences(path:String) = {  
    val file = new File(path)
    val sequences  = if(file.isDirectory()) 
      file.listFiles.toList.map(f => readFasta(f.getAbsolutePath)).flatten else readFasta(path)
    toSequenceNodes(sequences, Alphabet.guess(sequences.head._2)) 
  }  
 
  /** Write results to output file or stdout */
  def writeOutput(filepath:String, nodes:List[SequenceNode], predictions:List[(Pair,Double)]) {
    val writer = new BufferedWriter(if(filepath==STDOUT) new PrintWriter(stdout) else new FileWriter(filepath))
    val edge2score = predictions.map{case (e,s) => (Set(e._1,e._2),s)}.toMap
    def score(edge:Set[SequenceNode]) = 
      if(edge2score.contains(edge)) edge2score(edge).formatted("%.3f") else "0.000"
    for(n1 <- nodes) {
      val line = nodes.map(n2 => score(Set(n1,n2))).mkString(n1.name+" ","\t","\n")              
      writer.write(line)
    }
    if(filepath==STDOUT) writer.flush() else writer.close()
  }
    
  
  /** Write error messages to the log file */
  def writeError(message:String) {
    print(message)
    val writer = new BufferedWriter(new FileWriter("error.log"))
    writer.write(message)
    writer.close()
  }
  
  /** Return stack trace of exception as a string */
  def stackTrace(ex:Exception):String = {
    val sw = new StringWriter
    ex.printStackTrace(new PrintWriter(sw))
    sw.toString()
  }
  
  /** Guess n-gram and window size if not or only partially provided */
  def guess(nodes:List[SequenceNode], n:Int, w:Int) = {
    val alphabet = nodes.head.sequence.alphabet
    val nn = if(n<=0) { if(alphabet==DNA) 21 else 7 } else n
    val nw = if(w<=0) { if(alphabet==DNA) 60 else 20 } else w
    (nn,nw)
  }
  
  /** Parse the command line arguments and return a tuple with their values */
  def parseArgs(args:Array[String]) = {
    val usage = 
      "USAGE\t-i <fasta_file|folder> [-o <output_file|stdout> -n substring_length> -w <window-size> -m <high|medium|low> -p <profiles_file>]" +
      "\nEXAMPLE\tjava -jar LGTNet.jar -i sequences.fa -o network.tsv -p profiles.tsv"
    val pp = new ParseParms( usage )
    pp.parm("-i", "sequences.fa", "^.+$", true)   
      .parm("-o", "stdout", "^.+$")  
      .parm("-m", "high", "^high|medium|low$")  
      .parm("-n", "0","^\\d{1,2}$")        
      .parm("-w", "0","^\\d{1,3}$")   
      .parm("-p", "","^.+$") 
    val (success, msg, values) = pp.validate( args.toList )
    if(!success) throw new RuntimeException(msg) 
    (values("-i"), values("-n").toInt, values("-w").toInt, values("-m"), values("-o"), values("-p")) 
  }
   
  
  def main(args:Array[String]) {
    try {
      println("LGTNet Version: 1.00")
      val (infile,n,w,mode,outfile,profile) = parseArgs(args)
      println("loading ...")
      val nodes = readSequences(infile)   
      val (nn,nw) = guess(nodes,n,w)
      printf("parameters        %s\n",args.mkString(" "))
      printf("sequences read    %d\n", nodes.length)
      printf("alphabet          %s\n", nodes.head.sequence.alphabet)
      printf("substring length  %d\n", nn)
      printf("window size       %d\n", nw)
      printf("mode              %s\n", mode)
      print("processing")
      val profileWriter = if(profile.isEmpty()) null else new ProfileWriter(profile)
      val predictor = new LGTPredictorNGram(mode,nn,nw,profileWriter)
      val predictions = predictor.predict(nodes)
      println("\nwriting ...")
      writeOutput(outfile, nodes, predictions)    
      println("finished.")
    } catch {
      case ex:Exception => writeError(stackTrace(ex))
    }  
  }
}

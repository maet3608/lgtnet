package au.edu.imb.lgtnet


/**
 * Alphabet (e.g. Amino acids, Nucleotides) for molecular sequences
 */
abstract class Alphabet(val letters:Seq[Char]) extends Seq[Char] {
  private val letterSet = letters.toSet

  /** Returns index of letter for the given letter */
  val toIndex:Map[Char,Int] = letters.zipWithIndex.toMap

  /** Returns true if letter is in alphabet, false otherwise */
  def contains(letter:Char) = letterSet.contains(letter)

  /** Returns a random letter from the alphabet */
  def random = letters((scala.math.random*size).toInt)

  /** Number of letters within the alphabet */
  def length = letters.size

  /** Returns letter at index idx */
  def apply(idx:Int) = letters(idx)

  /** Iterator of alphabet letters */
  def iterator = letters.iterator

  /** Returns string representation */
  override def toString = letters.mkString(",")
}

object Alphabet {
   def guess(letters:String):Alphabet = if(letters.toSet.size > 8) AA else DNA
}


/** Nucleotide alphabet */
object DNA extends Alphabet("ACTG") {
  override def toString = "DNA"
}


/** Amino acid alphabet */
object AA extends Alphabet("ARNDCEQGHILKMFPSTWYV") {
  override def toString = "AA"
}


/** Usage example */
object AlphabetExample extends App {
  println(DNA)
  println(DNA contains 'A')
  println(AA)
}
package au.edu.imb.lgtnet.test

/**
 * Unit tests for alphabet class
 */

import org.scalatest.FunSuite
import au.edu.imb.lgtnet.{Alphabet,AA,DNA}


class AlphabetTest extends FunSuite  {

 object Nucleotides extends Alphabet("ACTG")

  test("constructor") {
    expectResult("A,C,T,G")(Nucleotides.toString)
  }

  test("length") {
    expectResult(4)(Nucleotides.length)
  }
  
  test("guess") {
    expectResult(AA)(Alphabet.guess("MPLLIAAGVVTR"))
    expectResult(DNA)(Alphabet.guess("ACTGACTGACTG"))
  }  

  test("contains") {
    expectResult(true)(Nucleotides.contains('A'))
    expectResult(true)(Nucleotides.contains('C'))
    expectResult(true)(Nucleotides.contains('T'))
    expectResult(true)(Nucleotides.contains('G'))
    expectResult(false)(Nucleotides.contains('X'))
  }

  test("toIndex") {
    expectResult(0)(Nucleotides.toIndex('A'))
    expectResult(1)(Nucleotides.toIndex('C'))
    expectResult(2)(Nucleotides.toIndex('T'))
    expectResult(3)(Nucleotides.toIndex('G'))
  }

  test("random") {
    val nucs = (0 to 4000) map (i => Nucleotides.random)
    val counts = nucs.groupBy(identity).values.map(_.length)
    expectResult(true)(counts.min > 900)
    expectResult(true)(counts.max < 1100)
  }
}

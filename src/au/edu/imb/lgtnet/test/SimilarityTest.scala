package au.edu.imb.lgtnet.test

import org.scalatest.FunSuite
import au.edu.imb.lgtnet.MolecularSequence
import au.edu.imb.lgtnet.Similarity
import au.edu.imb.lgtnet.DNA

/**
 * Unit tests for sequence similarity measures
 */
class SimilarityTest extends FunSuite  {

  test("hamming") {
    val s1 = MolecularSequence("AAAAAAAAAA", DNA)
    val s2 = MolecularSequence("AAAAAAAACC", DNA)
    expectResult(0.2)(Similarity.hamming(s1,s2))
  }
}
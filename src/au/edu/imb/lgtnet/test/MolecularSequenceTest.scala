package au.edu.imb.lgtnet.test

import org.scalatest.FunSuite
import au.edu.imb.lgtnet.MolecularSequence
import au.edu.imb.lgtnet.DNA


/**
 * Unit tests for molecular sequences
 */
class MolecularSequenceTest extends FunSuite  {

  test("constructor") {
    expectResult("ACTG")(MolecularSequence("ACTG", DNA).toString)
    expectResult("ACTG")(MolecularSequence("ACTG").toString)
  }

  test("length") {
    expectResult(8)(MolecularSequence("ACTGACTG", DNA).length)
  }

  test("slice") {
    val seq = MolecularSequence("ACTG", DNA)
    expectResult("AC")(seq(0,2).toString)
    expectResult("TG")(seq(2,4).toString)
  }

  test("index") {
    val seq = MolecularSequence("ACTG", DNA)
    expectResult('A')(seq(0))
    expectResult('G')(seq(3))
  }

  test("+") {
    expectResult("ACTGAACC")((MolecularSequence("ACTG", DNA)+"AACC").toString)
    expectResult("ACTGAACC")((MolecularSequence("ACTG",DNA)+MolecularSequence("AACC",DNA)).toString)
  }
}

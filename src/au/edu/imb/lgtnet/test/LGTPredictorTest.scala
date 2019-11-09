package au.edu.imb.lgtnet.test

import org.scalatest.FunSuite
import au.edu.imb.lgtnet.Metric.AUC
import au.edu.imb.lgtnet.MolecularSequence
import au.edu.imb.lgtnet.SequenceNode
import au.edu.imb.lgtnet.Metric
import au.edu.imb.lgtnet.LGTPredictorAlign
import au.edu.imb.lgtnet.LGTEvent
import au.edu.imb.lgtnet.DNA

/**
 * Unit tests for LGT predictors
 */
class LGTPredictorTest extends FunSuite  {

  implicit def string2seq(letters:String) = MolecularSequence(letters,DNA)

  // LGT event X->C
  def tree(withLGT:Boolean,t:Double = 0.0) =
    SequenceNode("R",0.0,  "CCCCCC",List(
      SequenceNode("X",t,  "ACCCCC",List(
        SequenceNode("A",t,"AACCCC",Nil),
        SequenceNode("B",t,"ACACCC",Nil)
      )),
      SequenceNode("Y",t,  "CCCCCT",List(
        SequenceNode("C",t,if(withLGT) "ACCCTT" else "CCCCTT", Nil),
        SequenceNode("D",t,"CCCTCT",Nil)
      ))
    ))

  test("LGTPredictorSimple") {
    import Metric.AUC
    val predictor = new LGTPredictorAlign("std","hm",2)
    val event = new LGTEvent("X",0.0, "C",0.0, null)
    expectResult(0.875) (predictor.performance(tree(true),event,AUC))
    expectResult(0.500) (predictor.performance(tree(false),event,AUC))
  }

}

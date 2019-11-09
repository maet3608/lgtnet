package au.edu.imb.lgtnet.test

import org.scalatest.FunSuite
import au.edu.imb.lgtnet.AbstractNode


/**
 * Unit test for tree name2node.
 */
class NodeTest extends FunSuite  {

  class Node(val name:String, val length:Double, val descendants:List[Node]) extends AbstractNode[Node] {
    def treeString:String =
      if(descendants.isEmpty) name else name + descendants.map(_.treeString).mkString("(",",",")")
    override def toString = name
  }

  object Node {
    def apply(name:String, descendants:List[Node]) = new Node(name,0.0,descendants)
    def apply(name:String) = new Node(name,0.0,Nil)
  }

  def tree1 = Node("R", List(Node("X",List(Node("A"),Node("B"))), Node("Y",List(Node("C"),Node("D")))))
  def tree2 = Node("R", List(Node("X",List(Node("A"),Node("B"))), Node("Y",List(Node("C")))))
  def tree3 = Node("R", List(Node("X",List(Node("B"))), Node("C")))

  val A = Node("A")
  val B = Node("B")
  val C = Node("C")
  val D = Node("D")
  val X = Node("X",List(A,B))
  val Y = Node("Y",List(C,D))
  val R = Node("R", List(X, Y))

  test("constructor") {
    expectResult("R(X(A,B),Y(C,D))")(tree1.treeString)
    expectResult("R(X(A,B),Y(C))")(tree2.treeString)
    expectResult("R(X(B),C)")(tree3.treeString)
  }

  test("leaves") {
    expectResult("A,B,C,D")(tree1.leaves.mkString(","))
    expectResult("A,B,C")(tree2.leaves.mkString(","))
    expectResult("B,C")(tree3.leaves.mkString(","))
  }

  test("path") {
    expectResult("R,X,A")(R.path(A).mkString(","))
    expectResult("R,X,B")(R.path(B).mkString(","))
    expectResult("R,Y,C")(R.path(C).mkString(","))
    expectResult("R,Y,D")(R.path(D).mkString(","))
    expectResult("R,X")(R.path(X).mkString(","))
    expectResult("R,Y")(R.path(Y).mkString(","))
    expectResult("R")(R.path(R).mkString(","))
  }

  test("allDescendants") {
    expectResult("R,X,A,B,Y,C,D")(R.allDescendants.mkString(","))
    expectResult("X,A,B")(X.allDescendants.mkString(","))
    expectResult("Y,C,D")(Y.allDescendants.mkString(","))
    expectResult("A")(A.allDescendants.mkString(","))
    expectResult("B")(B.allDescendants.mkString(","))
    expectResult("C")(C.allDescendants.mkString(","))
    expectResult("D")(D.allDescendants.mkString(","))
  }

  test("name2node") {
    expectResult(true)(tree3.name2node.contains("R"))
    expectResult(true)(tree3.name2node.contains("X"))
    expectResult(true)(tree3.name2node.contains("B"))
    expectResult(true)(tree3.name2node.contains("C"))
  }
}
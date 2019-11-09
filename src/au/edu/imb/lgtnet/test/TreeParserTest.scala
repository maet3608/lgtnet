package au.edu.imb.lgtnet.test

/**
 * Unit tests for Newick parser
 */

import org.scalatest.FunSuite
import au.edu.imb.lgtnet.SimpleParser
import au.edu.imb.lgtnet.NewickParser
import au.edu.imb.lgtnet.NewickParser2


// Simple node with a toString method that returns the node and its subtree.
protected case class TestNode(name:String, length:Double, descendants:List[TestNode]) {
  override def toString = name +
    (if(length !=0 ) ":"+length else "") +
    (if(descendants.isEmpty) "" else descendants.mkString("(",",",")"))
}


class SimpleParserTest extends FunSuite  {
  val parser = new SimpleParser(TestNode.apply)
  def parse(text:String) = parser.read(text).toString

  test("constructor") {
    assert(parser != null)
  }

  test("names") {
    expectResult("Name:0.1")(parse("Name:0.1"))
    expectResult("Name_1:0.1")(parse("Name_1:0.1"))
  }

  test("simple trees") {
    expectResult("A:0.1")(parse("A:0.1"))
    expectResult("A:0.1(B:0.2)")(parse("A:0.1 (B:0.2)"))
    expectResult("A:0.1(B:0.2,C:0.3)")(parse("A:0.1 (B:0.2 C:0.3)"))
    expectResult("R:0.1(X:0.2(A:0.3,B:0.4),Y:0.5(C:0.6,D:0.7))")(parse("R:0.1 (X:0.2 (A:0.3 B:0.4) Y:0.5 (C:0.6 D:0.7))"))
  }

  test("comments") {
    expectResult("A:0.1(B:0.2,C:0.3)")(parse("[a comment] A:0.1 (B:0.2 C:0.3)"))
    expectResult("A:0.1(B:0.2,C:0.3)")(parse("A:0.1 (B:0.2 C:0.3) [a comment]"))
    expectResult("A:0.1(B:0.2,C:0.3)")(parse("A:0.1 [a comment] (B:0.2 C:0.3)"))
    expectResult("A:0.1(B:0.2)")(parse("A:0.1 [a comment] (B:0.2 [C:0.3])"))
    expectResult("A:0.1")(parse("A:0.1 [(B:0.2 C:0.3)]"))
  }
}


class NewickParserTest extends FunSuite  {
  val parser = new NewickParser(TestNode.apply)
  def parse(text:String) = parser.read(text).toString

  test("constructor") {
    assert(parser != null)
  }

  test("simple trees without branch lengths") {
    expectResult("A")(parse("A;"))
    expectResult("A(B)")(parse("(B)A;"))
    expectResult("C(A,B)")(parse("(A,B)C;"))
    expectResult("F(A,E(B,C))")(parse("(A,(B,C)E)F;"))
    expectResult("F(E((B,C),A))")(parse("(((B,C),A)E)F;"))
  }

  test("simple trees without names") {
    expectResult("")(parse(";"))
    expectResult("()")(parse("();"))
    expectResult("(,)")(parse("(,);"))
    expectResult("(,(,))")(parse("(,(,));"))
    expectResult("(((,),))")(parse("(((,),));"))
  }

  test("simple trees with branch lengths") {
    expectResult("A:0.1")(parse("A:0.1;"))
    expectResult("A:0.1(B:0.2)")(parse("(B:0.2)A:0.1;"))
    expectResult("C:0.3(A:0.1,B:0.2)")(parse("(A:0.1,B:0.2)C:0.3;"))
    expectResult("F:0.5(A:0.1,E:0.4(B:0.2,C:0.3))")(parse("(A:0.1,(B:0.2,C:0.3)E:0.4)F:0.5;"))
    expectResult("F:0.5(E:0.4((B:0.2,C:0.3),A:0.1))")(parse("(((B:0.2,C:0.3),A:0.1)E:0.4)F:0.5;"))
  }

  test("simple trees") {
    expectResult("(,,(,))")(parse("(,,(,));"))
    expectResult("(A,B,(C,D))")(parse("(A,B,(C,D));"))
    expectResult("F(A,B,E(C,D))")(parse("(A,B,(C,D)E)F;"))
    expectResult("(:0.1,:0.2,:0.5(:0.3,:0.4))")(parse("(:0.1,:0.2,(:0.3,:0.4):0.5);"))
    expectResult("(:0.1,:0.2,:0.5(:0.3,:0.4))")(parse("(:0.1,:0.2,(:0.3,:0.4):0.5):0.0;"))
    expectResult("(A:0.1,B:0.2,:0.5(C:0.3,D:0.4))")(parse("(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);"))
    expectResult("F(A:0.1,B:0.2,E:0.5(C:0.3,D:0.4))")(parse("(A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F;"))
    expectResult("A(F:0.1(B:0.2,E:0.5(C:0.3,D:0.4)))")(parse("((B:0.2,(C:0.3,D:0.4)E:0.5)F:0.1)A;"))
  }

  test("names unquoted") {
    expectResult("Name")(parse("Name;"))
    expectResult("Name_1")(parse("Name_1;"))
    expectResult("Name:0.1")(parse("Name:0.1;"))
  }

  test("names quoted") {
    expectResult("Name[(,")(parse("'Name[(,';"))
    expectResult("My Name's")(parse("'My Name''s';"))
    expectResult("Name:0.1:0.2")(parse("'Name:0.1':0.2;"))
    expectResult("Name with blanks(Name(),Name,name)")(parse("('Name()','Name,name')'Name with blanks';"))
  }

  test("branch lengths") {
    expectResult("A:1.0")(parse("A:1;"))
    expectResult("A:0.1")(parse("A:0.1;"))
    expectResult("A:-0.1")(parse("A:-0.1;"))
    expectResult("A:0.001")(parse("A:1.e-3;"))
    expectResult("A:-1000.0")(parse("A:-1.e+3;"))
    expectResult("A:-0.102")(parse("A:-1.02e-01;"))
  }

  test("comments") {
    expectResult("C:0.3(A:0.1,B:0.2)")(parse("(A:0.1, [comment] B:0.2)C:0.3;"))
    expectResult("(A:0.1,B:0.2)")(parse("(A:0.1, [comment] B:0.2) [C:0.3];"))
    expectResult("(A:0.1,B:0.2)")(parse("(A:0.1, [comment,;(] B:0.2) [C:0.3];"))
  }

  test("complex trees") {
    expectResult("the root:5.0(a,:-1230.0( Names's and (b's)) ,c))")(parse("(a , (' Names''s and (b''s)) ',c) :-1.23e3) 'the root':5;"))
  }
}


class NewickParser2Test extends FunSuite  {
  val parser = new NewickParser2(TestNode.apply)
  def parse(text:String) = parser.read(text).toString

  test("constructor") {
    assert(parser != null)
  }

  test("names quoted") {
    expectResult("Name[(,")(parse("'Name[(,';"))
    expectResult("Name[1](2):0.2")(parse("'Name[1](2)'[:0.1]:0.2;"))
  }

  test("nested comments") {
    expectResult("C:0.3(A(1)[2]:0.1,B:0.2)")(parse("('A(1)[2]':0.1, [comment [with in] ] B:0.2)C:0.3;"))
  }
}


class TreeParserTest extends FunSuite {
  (new SimpleParserTest)
  (new NewickParserTest)
  (new NewickParser2Test)
}


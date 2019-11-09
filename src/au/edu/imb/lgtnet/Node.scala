package au.edu.imb.lgtnet

/**
 * Nodes within a phylogenetic tree.
 */


/** Abstract super class of name2node that can display the tree */
trait AbstractNode[+T]{
  val name:String
  val length:Double
  val descendants:List[AbstractNode[T]]


  /** Returns a list of the leaves of the sub tree belonging to this node */
  def leaves:List[T] = {
    def traverse(node:AbstractNode[T]):List[T] =
      if(node.descendants.isEmpty) List(node.asInstanceOf[T]) else node.descendants.map(traverse).flatten
    traverse(this)
  }

  /** Returns a map from node.name to node for all name2node within the subtree */
  def name2node:Map[String,T] = {
    def traverse(node:AbstractNode[T]):List[(String,T)] =
      (node.name,node.asInstanceOf[T])::node.descendants.map(traverse).flatten
    Map(traverse(this):_*)
  }

  /** Returns a list of all descendants of this node including this node */
  def allDescendants:List[T] = {
    def traverse(node:AbstractNode[T]):List[T] =
      node.asInstanceOf[T]::node.descendants.map(traverse).flatten
    traverse(this)
  }

  /** Returns path from this node to the destination node  */
  def path(dest:AbstractNode[_]):List[T] = {
    def traverse(node:AbstractNode[T]):List[T] = {
      if(node==dest) return List(node.asInstanceOf[T])
      val nodes = node.descendants.map(traverse).flatten
      if(nodes.isEmpty) List[T]() else node.asInstanceOf[T]::nodes
    }
    traverse(this)
  }

  /** Prints the node and its subtree */
  def display(indent:Int=2, level:Int=0) {
    println(" "*level + toString)
    descendants.foreach(_.display(indent,level+indent))
  }
}


/**
 * Default name2node as created by a tree parser.
 * @param name  Node name
 * @param length Branch length (to parent)
 * @param descendants Children of the node
 */
case class DefaultNode(name:String, length:Double, descendants:List[DefaultNode]=Nil)
  extends AbstractNode[DefaultNode] {
  override def toString = "%s:%.2f" format (name,length)
}


/**
 * Nodes with a sequence as generated by an evolver
 * @param name  Node name
 * @param length Branch length (to parent)
 * @param sequence Molecular sequence
 * @param descendants Children of the node
 */
case class SequenceNode(name:String, length:Double, sequence:MolecularSequence, descendants:List[SequenceNode])
  extends AbstractNode[SequenceNode] {
  override def toString = "%s:%.2f:%s" format (name,length,sequence)
}


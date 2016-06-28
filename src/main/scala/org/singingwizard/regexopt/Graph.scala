package org.singingwizard.regexopt

abstract class Graph[E] {
  orig =>
  import Graph._
  type Edge = EdgeT[E]
  val Edge = EdgeT

  val nodes: Set[Node]
  val edges: Set[Edge]

  def edgesFrom(n: Node): Set[Edge] = edges.filter(_.s == n)
  def edgesTo(n: Node): Set[Edge] = edges.filter(_.t == n)

  def ++(e: Iterable[Edge]): Graph[E] = new Graph[E] {
    val nodes = orig.nodes
    val edges = orig.edges ++ e
  }
  def +(e: Edge): Graph[E] = new Graph[E] {
    val nodes = orig.nodes
    val edges = orig.edges + e
  }
  def -(e: Edge): Graph[E] = new Graph[E] {
    val nodes = orig.nodes
    val edges = orig.edges - e
  }
  def --(e: Iterable[Edge]): Graph[E] = new Graph[E] {
    val nodes = orig.nodes
    val edges = orig.edges -- e
  }

  def +(n: Node): Graph[E] = new Graph[E] {
    val nodes = orig.nodes + n
    val edges = orig.edges
  }
  def -(n: Node): Graph[E] = new Graph[E] {
    val nodes = orig.nodes - n
    val edges = orig.edges
  }

  def merge(a: Node, b: Node): Graph[E] = {
    if (a == b) {
      this
    } else {
      def replace(n: Node) = if (n == a) b else n
      new Graph[E] {
        val nodes = orig.nodes - a
        val edges = for (Edge(s, l, t) <- orig.edges) yield Edge(replace(s), l, replace(t))
      }
    }
  }
  
  def toDOT() = {
    def escape(s: String) = s.replace("\\", "\\\\")
    val nodesStr = (for((node, id) <- nodes zipWithIndex) yield {
      s"""  $node [label="${node.name.getOrElse(id)}"];"""
    }).mkString("\n")
    val edgesStr = (for(Edge(s, l, e) <- edges) yield {
      s"""  $s -> $e [label="${escape(l.toString)}"];"""
    }).mkString("\n")
    s"""digraph {\n$nodesStr\n$edgesStr\n}\n"""
  }
  
  override def toString() = {
    val edgesStr = (for(Edge(s, l, e) <- edges) yield {
      s"""$s -($l)-> $e"""
    }).mkString("\n")
    edgesStr
  }
  
  def equals(o: AnyCharacter): Boolean = {
    o.asInstanceOf[AnyRef] match {
      case g: Graph[E] => edges == g.edges && nodes == g.edges
      case _ => false
    }
  }
}

object Graph {
  class Node {
    private[Graph] var id = hashCode()
    private[Graph] var _name: String = null
    private def idOrName: String = if(_name ne null) _name else id.toString
    override def toString() = s"n$idOrName"
    
    def setName(s: String): Unit = {
      require(_name eq null)
      _name = s
    }
    
    def name = Option(_name)
  }
  case class EdgeT[E](s: Node, l: E, t: Node) extends PrecomputeHashcode

  def apply[E]() = new Graph[E] {
    val nodes = Set[Node]()
    val edges = Set[Edge]()
  }
}

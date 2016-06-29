package org.singingwizard.regexopt.mutable

import scala.collection.mutable
import org.singingwizard.regexopt.PrecomputeHashcode

class Graph[E] {
  import Graph._
  type Edge = EdgeT[E]
  val Edge = EdgeT

  val nodes = new mutable.HashSet[Node]()
  val edges = new mutable.HashSet[Edge]()
  
  val edgesToIndex = new mutable.HashMap[Node, mutable.HashSet[Edge]]()
  val edgesFromIndex = new mutable.HashMap[Node, mutable.HashSet[Edge]]()

  def edgesFrom(n: Node): collection.Set[Edge] = edgesFromIndex(n)
  def edgesTo(n: Node): collection.Set[Edge] = edgesToIndex(n)

  def ++=(es: Iterable[Edge]): Unit = {
    for (e <- es) {
      this += e
    }
  }
  def +=(e: Edge): Unit = {
    edges += e
    edgesFromIndex(e.s) += e
    edgesToIndex(e.t) += e
  }
  
  //def ++=(e: Iterable[Node]): Unit = nodes ++= e
  def +=(n: Node): Unit = {
    nodes += n
    edgesFromIndex(n) = new mutable.HashSet[Edge]()
    edgesToIndex(n) = new mutable.HashSet[Edge]()
  }

  def --=(es: Iterable[Edge]): Unit = {
    for (e <- es) {
      this -= e
    }
  }
  def -=(e: Edge): Unit = {
    edges -= e
    edgesFromIndex(e.s) -= e
    edgesToIndex(e.t) -= e
  }
  
  //def --=(e: Iterable[Node]): Unit = nodes --= e
  def -=(n: Node): Unit = {
    nodes -= n
    edgesFromIndex.remove(n)
    edgesToIndex.remove(n)
  }

  def merge(a: Node, b: Node): Unit = {
    if (a == b) {
    } else {
      for (eo@Edge(s, l, t) <- edgesFrom(a)) {
        assert(s == a)
        this -= eo
        this += Edge(b, l, t)
      }
      for (eo@Edge(s, l, t) <- edgesTo(a)) {
        assert(t == a)
        this -= eo
        this += Edge(s, l, b)
      }
      this -= a
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
}

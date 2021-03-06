package org.singingwizard.regexopt.immutable
import org.singingwizard.regexopt.toFixedPoint

/** The goal of this object is to reduce a graph such that is still has
  * all the same paths (in terms of edge labels), but has fewer nodes and edges.
  */
object GraphReduction {
  def partitionNodesByInEdges[E](g: Graph[E]) =
    g.nodes.groupBy { n =>
      g.edgesTo(n).map {
        case g.Edge(s, l, t) => (s, l)
      }
    }
  def partitionNodesByOutEdges[E](g: Graph[E]) =
    g.nodes.groupBy { n =>
      g.edgesFrom(n).map {
        case g.Edge(s, l, t) => (l, t)
      }
    }

  def mergeNodeSet[E](g: Graph[E], ns: Set[Graph.Node]) = {
    val target = ns.head
    ns.foldLeft(g) { (g, n) =>
      g.merge(n, target)
    }
  }

  def mergeNodesByInEdges[E](g: Graph[E]) = {
    val nodeSets = partitionNodesByInEdges(g).values
    nodeSets.foldLeft(g) { (g, ns) => mergeNodeSet(g, ns) }
  }

  def mergeNodesByOutEdges[E](g: Graph[E]) = {
    val nodeSets = partitionNodesByOutEdges(g).values
    nodeSets.foldLeft(g) { (g, ns) => mergeNodeSet(g, ns) }
  }

  def apply[E](g: Graph[E]) = {
    toFixedPoint(g) { (g: Graph[E]) =>
      mergeNodesByOutEdges(mergeNodesByInEdges(g))
    }
  }
}
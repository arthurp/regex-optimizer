package org.singingwizard.regexopt.mutable

import org.singingwizard.regexopt.untilFalse

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

  def mergeNodeSet[E](g: Graph[E], ns: Iterable[Graph.Node]) = {
    val target = ns.head
    for (n <- ns) {
      g.merge(n, target)
    }
  }

  def mergeNodesByInEdges[E](g: Graph[E]) = {
    val nodeSets = partitionNodesByInEdges(g).values
    for (ns <- nodeSets) {
      mergeNodeSet(g, ns)
    }
    nodeSets.size > 0
  }

  def mergeNodesByOutEdges[E](g: Graph[E]) = {
    val nodeSets = partitionNodesByOutEdges(g).values
    for (ns <- nodeSets) {
      mergeNodeSet(g, ns)
    }
    nodeSets.size > 0
  }

  def apply[E](g: Graph[E]) = {
    untilFalse() {
      mergeNodesByInEdges(g) ||
      mergeNodesByOutEdges(g)
    }
  }
}
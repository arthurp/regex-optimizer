package org.singingwizard.regexopt.immutable
import org.singingwizard.regexopt._

object RegexGraph {
  def apply(r: Regex): Graph[Regex] = {
    case class TState(startNode: Graph.Node, endNode: Graph.Node, graph: Graph[Regex])
    
    def process(r: Regex, s: TState): Graph[Regex] = {
      r match {
        case Sequence(rs) => {
          val TState(start, end, g) = rs.foldLeft(s) { (s, r) =>
            val next = new Graph.Node()
            val g = s.graph + next
            s.copy(graph = process(r, s.copy(graph = g, endNode = next)), startNode = next)
          }
          val merged = g.merge(start, s.endNode)
          merged
        }
        case Alternative(rs) => {
          val TState(start, end, _) = s
          rs.foldLeft(s.graph) { (g, r) =>
            process(r, TState(start, end, g))
          }
        }
        case Group(r, "") => {
          process(r, s)
        }
        case _ => s.graph + s.graph.Edge(s.startNode, r, s.endNode)
      }
    }

    val start = new Graph.Node()
    start.setName("start")
    val end = new Graph.Node()
    end.setName("end")
    val g = Graph[Regex]() + start + end

    process(r, TState(start, end, g))
  }
}

object GraphRegex {
  implicit class GraphExt(val outer: Graph[Regex]) extends AnyVal {
    import outer._
    def insertEdge(e: Edge): Graph[Regex] = new Graph[Regex] {
      val nodes = outer.nodes
      val edges = outer.edges.find(e2 => e.s == e2.s && e.t == e2.t) match {
        case None     => outer.edges + e
        case Some(e2) => outer.edges - e2 + Edge(e2.s, Alternative(e2.l, e.l), e2.t)
      }
    }
    def mergeEdges(es: Iterable[Edge]): Graph[Regex] = {
      if (es.size <= 1) {
        outer
      } else {
        assert(es.forall { _.s == es.head.s })
        assert(es.forall { _.t == es.head.t })
        new Graph[Regex] {
          val nodes = outer.nodes
          val edges = outer.edges -- es + Edge[Regex](es.head.s, Alternative(es.map(_.l).toSeq), es.head.t)
        }
      }
    }
    def ripNode(n: Graph.Node): Graph[Regex] = {
      val ins = edgesTo(n)
      val outs = edgesFrom(n)

      val (oldEdges, newEdges) = (for (i <- ins; o <- outs) yield (Seq(i, o), Edge[Regex](i.s, Sequence(i.l, o.l), o.t))).unzip

      outer -- oldEdges.flatten ++ newEdges - n
    }
    def ripNodeCost(n: Graph.Node): Int = {
      val ins = edgesTo(n)
      val outs = edgesFrom(n)

      if (ins.size > 0 && outs.size > 0) {
        (ins.size - 1) * outs.map(_.l.cost).sum +
          (outs.size - 1) * ins.map(_.l.cost).sum
      } else {
        Int.MaxValue
      }
    }
  }

  def mergeParallelEdges(g: Graph[Regex]) = {
    val edgesByEnds = g.edges.groupBy { e => (e.s, e.t) }.values

    edgesByEnds.foldLeft(g)((g, es) => g.mergeEdges(es))
  }

  def simplify(g: Graph[Regex]): Graph[Regex] = {
    /* Remove nodes one at a time by creating edges that are labeled with the 
     * concatenation of the edges in and out of the removed node. The edge is
     * combined with the existing edges using alternative.  
     * 
     * This is correct however the quality of the result is extremely sensetive
     * to the order in which nodes are removed.
     * 
     * It is not clear what heuristic will provide useful results.
     */

    def findNochoiceNode(g: Graph[Regex]) = {
      g.nodes.find(n => {
        g.edgesFrom(n).size == 1 && g.edgesTo(n).size == 1
      })
    }

    toFixedPoint(g) { (g: Graph[Regex]) =>
      findNochoiceNode(g).map(n => {
        println(s"ripping $n cost ${g.ripNodeCost(n)}")
        mergeParallelEdges(g.ripNode(n))
      }).getOrElse(g)
    }
  }

  def reduceToOneEdge(g: Graph[Regex]) = {
    val Some(start) = g.nodes.find(g.edgesTo(_).size == 0)
    val Some(end) = g.nodes.find(g.edgesFrom(_).size == 0)

    toFixedPoint(g) { (g: Graph[Regex]) =>
      val n = g.nodes.minBy(g.ripNodeCost(_))
      if (Set(start, end) contains n) {
        g
      } else {
        println(s"ripping $n cost ${g.ripNodeCost(n)}")
        mergeParallelEdges(g.ripNode(n))
      }
    }
  }

  def apply(g: Graph[Regex]) = {
    val g1 = reduceToOneEdge(g)

    assert(g1.edges.size == 1)

    g1.edges.head.l
  }
}
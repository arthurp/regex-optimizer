package org.singingwizard.regexopt.mutable
import org.singingwizard.regexopt._

object RegexGraph {
  def apply(r: Regex): Graph[Regex] = {
    val start = new Graph.Node()
    start.setName("start")
    val end = new Graph.Node()
    end.setName("end")
    val g = new Graph[Regex]()
    g += start 
    g += end

    case class TState(startNode: Graph.Node, endNode: Graph.Node)
    
    def process(r: Regex, s: TState): Unit = {
      r match {
        case Sequence(rs) => {
          val TState(start, end) = rs.foldLeft(s) { (s, r) =>
            val next = new Graph.Node()
            g += next
            process(r, s.copy(endNode = next))
            s.copy(startNode = next)
          }
          g.merge(start, s.endNode)
        }
        case Alternative(rs) => {
          val TState(start, end) = s
          for (r <- rs) {
            process(r, TState(start, end))
          }
        }
        case Group(r, "") => {
          process(r, s)
        }
        case _ => 
          g += g.Edge(s.startNode, r, s.endNode)
      }
    }

    process(r, TState(start, end))
    
    g
  }
}

object GraphRegex {
  implicit class GraphExt(val g: Graph[Regex]) extends AnyVal {
    import g._
    def insertEdge(e: Edge): Boolean = {
      edgesFrom(e.s).find(e2 => e.t == e2.t) match {
        case None     => {
          g += e
          true
        }
        case Some(e2) => {
          g -= e2
          g += Edge[Regex](e2.s, Alternative(e2.l, e.l), e2.t)
          true
        }
      }
    }
    def mergeEdges(es: Iterable[Edge]): Boolean = {
      if (es.size <= 1) {
        false
      } else {
        assert(es.forall { _.s == es.head.s })
        assert(es.forall { _.t == es.head.t })
        g --= es
        g += Edge[Regex](es.head.s, Alternative(es.map(_.l).toSeq), es.head.t)
        es.size > 0
      }
    }
    def ripNode(n: Graph.Node): Boolean = {
      val ins = edgesTo(n)
      val outs = edgesFrom(n)

      val (oldEdges, newEdges) = (for (i <- ins; o <- outs) yield (Seq(i, o), Edge[Regex](i.s, Sequence(i.l, o.l), o.t))).unzip

      g --= oldEdges.flatten
      g ++= newEdges
      g -= n
      
      true
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

    edgesByEnds.foreach(g.mergeEdges(_))
  }

  def simplify(g: Graph[Regex]): Unit = {
    /* Remove nodes one at a time by creating edges that are labeled with the 
     * concatenation of the edges in and out of the removed node. The edge is
     * combined with the existing edges using alternative.  
     * 
     * This is correct however the quality of the result is extremely sensetive
     * to the order in which nodes are removed.
     * 
     * It is not clear what heuristic will provide useful results.
     */

    def findNochoiceNode() = {
      g.nodes.find(n => {
        g.edgesFrom(n).size == 1 && g.edgesTo(n).size == 1
      })
    }

    untilFalse() {
      findNochoiceNode().map(n => {
        println(s"ripping $n cost ${g.ripNodeCost(n)}")
        val r = g.ripNode(n)
        mergeParallelEdges(g)
        r
      }).getOrElse(false)
    }
  }

  def reduceToOneEdge(g: Graph[Regex]) = {
    val Some(start) = g.nodes.find(g.edgesTo(_).size == 0)
    val Some(end) = g.nodes.find(g.edgesFrom(_).size == 0)

    untilFalse() {
      val n = g.nodes.minBy(g.ripNodeCost(_))
      if (Set(start, end) contains n) {
        false
      } else {
        println(s"ripping $n cost ${g.ripNodeCost(n)}")
        g.ripNode(n)
        mergeParallelEdges(g)
        true
      }
    }
  }

  def apply(g: Graph[Regex]) = {
    reduceToOneEdge(g)

    assert(g.edges.size == 1)

    g.edges.head.l
  }
}
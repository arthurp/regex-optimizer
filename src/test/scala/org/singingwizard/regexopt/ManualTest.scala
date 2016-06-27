package org.singingwizard.regexopt
import scala.io.Source

object ManualTest {

  def main(args: Array[String]) = {
    val lines = Seq("abas", "acas") ++ Source.fromInputStream(getClass().getResourceAsStream("test-input-ext.glob")).getLines()
    val regexs = for (line <- lines if line != "") yield GlobParser.parse(line)

    val r = Alternative(regexs.toSeq)
    println(r)
    val g = RegexGraph(r)
    println(g)
    println("------")
    println(GraphReduction.partitionNodesByInEdges(g).mkString("\n"))
    println("------")
    println(GraphReduction.partitionNodesByOutEdges(g).mkString("\n"))
    println("------")
    println(GraphReduction.mergeNodesByInEdges(g))
    println("------")
    println(GraphReduction.mergeNodesByOutEdges(g))
    println("------")
    val g1 = GraphReduction(g)
    println(g1)
    val dot = g1.toDOT()
    Graphvis.display(dot)
  }
}
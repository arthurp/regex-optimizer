package org.singingwizard.regexopt
import scala.io.Source
import org.singingwizard.regexopt.immutable._

object ManualTest {

  def main(args: Array[String]) = {
    val src = if (args.size >= 1) {
      Source.fromFile(args(0))
    } else {
      Source.fromInputStream(getClass().getResourceAsStream("test-input-ext.glob"))
    }
    
    val lines = src.getLines().take(200)
    val regexs = (for (line <- lines if line != "") yield {
      GlobParser.parseOpt(line) match {
        case f@GlobParser.NoSuccess(msg, _) => {
          println(s"parse failed on '$line' with $msg")
          None
        }
        case GlobParser.Success(r, _) => Some(r) 
      }
    }).flatten.toSeq

    val r = Alternative(regexs)
    println("Building graph...")
    val g = RegexGraph(r)
    println("Reducing graph...")
    val g0 = GraphReduction(g)
    /*
    println(r)
    println(g)
    val g1 = GraphRegex.simplify(g0)
    */
    //val g2 = GraphRegex.reduceToOneEdge(g1)
    println("Building final regex...")
    val r2 = GraphRegex(g0)
    /*
    println("------")
    val dot = g1.toDOT()
    Graphvis.display(dot)
    */
    println(r2.asInstanceOf[Alternative].rs.mkString("|\n"))
  }
}
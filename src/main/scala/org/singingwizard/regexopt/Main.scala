package org.singingwizard.regexopt
import scala.io.Source
import org.singingwizard.regexopt.mutable._

object Main {
  def main(args: Array[String]) = {
    val src = Source.fromFile(args(0))
    
    val lines = src.getLines()
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
    GraphReduction(g)
    println("Building final regex...")
    val r2 = GraphRegex(g)
    println(r2.asInstanceOf[Alternative].rs.mkString("|\n"))
  }
}
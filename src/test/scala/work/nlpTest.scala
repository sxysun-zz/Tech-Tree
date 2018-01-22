package test.scala.work

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import main.scala.nlp._
import main.scala.util._

object nlpTest {
  
  def main(args: Array[String]): Unit = {
    
    val USER_SEARCH = cleanUpUserInput("Type theory")
    
    //diskOperator.writeToLocalDisk("./test.txt", ("" /: semanticsAnalysis("./a.txt").analyzed) (_++_))
    val filtered = featureExtraction("./sample/"+USER_SEARCH+".txt").filteredTypedList
    println(filtered)
    
    semanticsAnalysis.analysisInst.load(
        semanticsAnalysis.trainedModelPath, Int.MaxValue, true)
        
    //println(semanticsAnalysis.analysisInst.cosine("math", "computer"))
  }
  
  def cleanUpUserInput(input: String): String = 
    input.replace("\n", "").replace("\r", "").replace(" ", "")
  
}
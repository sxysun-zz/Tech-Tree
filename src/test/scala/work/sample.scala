package test.scala.work

import main.scala.scraper._
import main.scala.nlp._
import java.io._
import main.scala.util._

/**
 * object used to extract raw data in all academic subjects on Net
 */
object work {
  def main(args: Array[String]): Unit = {
  //println(featureExtraction("./sample/acm10.txt").filteredTypedList)
    
    val CURRENTINDEX = 200 // starts at ONE
    val reader = new BufferedReader(new FileReader(new File("./academicSubjects.txt")))
    def loadTextTail(s: String, n: Int): Unit = s match {
      case null => println("Conmpleted")
      case _ => {
        val line = reader.readLine().replace(" ", "").replace("\n", "").replace("\r", "")
        println(n + " --- " + line)
        if(line != "" && n >= CURRENTINDEX) {
          diskOperator.writeToLocalDisk("./sample/"+line+".txt", 
              googleScraper(line, 1).getContent())
        }
        loadTextTail(line, n+1)
      }
    }
    loadTextTail(reader.readLine(), 0)
  }
}

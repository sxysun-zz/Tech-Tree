package main.scala.nlp

import edu.stanford.nlp.simple._
import main.scala.util._
import com.kennycason.kumo.WordFrequency
import com.kennycason.kumo.nlp._
import java.io._

/**
 * the first level of Natural language processing
 * @param location of the document containing raw data
 */
case class featureExtraction (path: String){
  val minWordLength = 4
  val numTermsToReturn = 100
  
  private val frequencyAnalyzer = new FrequencyAnalyzer();
  frequencyAnalyzer.setWordFrequenciesToReturn(numTermsToReturn);
  frequencyAnalyzer.setMinWordLength(minWordLength);
  private val wordFrequencies = frequencyAnalyzer.load(path);
  private val terminate = wordFrequencies.size()
  
  private val extractedList: List[WordFrequency] = 
    conversion.javaListToScalaList(wordFrequencies)
  
  /**
   * list containing the extracted features and their semantic types, second is type
   */
  val filteredTypedList = extractedList.map(
          x => {
            val tupl2 = new Sentence(
              x.getWord).parse().getChildrenAsList.get(0).getChildrenAsList.get(0)
            Tuple2(tupl2.getChild(0).toString(), 
                tupl2.value())
          }
        ).filter(_._2 == "NN")
  
  private def loadRawText(path: String) = {
    val reader = new BufferedReader(new FileReader(new File(path)))
    val builder: StringBuilder = new StringBuilder("")
    def loadTextTail(s: String, o: StringBuilder): String = s match {
      case null => o.toString()
      case _ => loadTextTail(reader.readLine(), o.append(s))
    }
    loadTextTail(reader.readLine(), builder)
  }
}
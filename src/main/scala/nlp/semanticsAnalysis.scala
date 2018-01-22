package main.scala.nlp

import edu.stanford.nlp.simple._
import main.scala.util.diskOperator
import main.scala.util.conversion
import java.io._
import scala.Array
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * the second level of natural language processing
 */
object semanticsAnalysis {
  
  /**
   * port to use the trained word2vec model to process
   */
  val analysisInst = new word2VecNLP()
  
  /**
   * the binary file containing trained document vectors
   */
  val trainedModelPath = "./res/vectors.bin"
  
}
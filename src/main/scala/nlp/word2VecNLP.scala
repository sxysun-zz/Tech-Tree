package main.scala.nlp

import edu.stanford.nlp.simple._
import main.scala.util.diskOperator
import main.scala.util.conversion
import java.io._
import scala.Array
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * original code from https://github.com/trananh/word2vec-scala under Apache-2.0
 * code slightly changed by Alayi
 */

class word2VecNLP {

  private val vocab = new mutable.HashMap[String, Array[Float]]()

  private var numWords = 0

  private var vecSize = 0

  def load(filename: String, limit: Integer = Int.MaxValue, normalize: Boolean = true): Unit = {
    val file = new File(filename)
    if (!file.exists()) {
      throw new FileNotFoundException("Binary vector file not found <" + file.toString + ">")
    }

    val reader = new VecBinaryReader(file)

    numWords = Integer.parseInt(reader.readToken())
    vecSize = Integer.parseInt(reader.readToken())
    println("\nFile contains " + numWords + " words with vector size " + vecSize)

    var word = ""
    val vector = new Array[Float](vecSize)
    var normFactor = 1f
    for (_ <- 0 until math.min(numWords, limit)) {
      word = reader.readToken()

      for (i <- 0 until vector.length) vector(i) = reader.readFloat()

      normFactor = if (normalize) magnitude(vector).toFloat else 1f
      vocab.put(word, vector.map(_ / normFactor) )
      
      reader.read()
    }
    println("Loaded " + math.min(numWords, limit) + " words.\n")

    reader.close()
  }

  def wordsCount: Int = numWords

  def vectorSize: Int = vecSize

  def clear() {
    vocab.clear()
    numWords = 0
    vecSize = 0
  }

  def contains(word: String): Boolean = {
    vocab.get(word).isDefined
  }

  def vector(word: String): Array[Float] = {
    vocab.getOrElse(word, Array[Float]())
  }

  def euclidean(vec1: Array[Float], vec2: Array[Float]): Double = {
    assert(vec1.length == vec2.length, "Uneven vectors!")
    var sum = 0.0
    for (i <- 0 until vec1.length) sum += math.pow(vec1(i) - vec2(i), 2)
    math.sqrt(sum)
  }

  def euclidean(word1: String, word2: String): Double = {
    assert(contains(word1) && contains(word2), "Out of dictionary word! " + word1 + " or " + word2)
    euclidean(vocab.get(word1).get, vocab.get(word2).get)
  }

  def cosine(vec1: Array[Float], vec2: Array[Float]): Double = {
    assert(vec1.length == vec2.length, "Uneven vectors!")
    var dot, sum1, sum2 = 0.0
    for (i <- 0 until vec1.length) {
      dot += (vec1(i) * vec2(i))
      sum1 += (vec1(i) * vec1(i))
      sum2 += (vec2(i) * vec2(i))
    }
    dot / (math.sqrt(sum1) * math.sqrt(sum2))
  }

  def cosine(word1: String, word2: String): Double = {
    assert(contains(word1) && contains(word2), "Out of dictionary word! " + word1 + " or " + word2)
    cosine(vocab.get(word1).get, vocab.get(word2).get)
  }

  def magnitude(vec: Array[Float]): Double = {
    math.sqrt(vec.foldLeft(0.0){(sum, x) => sum + (x * x)})
  }

  def normalize(vec: Array[Float]): Array[Float] = {
    val mag = magnitude(vec).toFloat
    vec.map(_ / mag)
  }

  def sumVector(input: List[String]): Array[Float] = {
    // Find the vector representation for the input. If multiple words, then aggregate (sum) their vectors.
    input.foreach(w => assert(contains(w), "Out of dictionary word! " + w))
    val vector = new Array[Float](vecSize)
    input.foreach(w => for (j <- 0 until vector.length) vector(j) += vocab.get(w).get(j))
    vector
  }

  def nearestNeighbors(vector: Array[Float], inSet: Option[Set[String]] = None,
                       outSet: Set[String] = Set[String](), N: Integer = 40)
  : List[(String, Float)] = {
    val top = new mutable.PriorityQueue[(String, Float)]()(Ordering.by(-_._2))

    var dist = 0f
    val iterator = if (inSet.isDefined) vocab.filterKeys(k => inSet.get.contains(k)).iterator else vocab.iterator
    iterator.foreach(entry => {
      if (!outSet.contains(entry._1)) {
        dist = cosine(vector, entry._2).toFloat
        if (top.size < N || top.head._2 < dist) {
          top.enqueue((entry._1, dist))
          if (top.length > N) {
            top.dequeue()
          }
        }
      }
    })

    assert(top.length <= N)
    top.toList.sortWith(_._2 > _._2)
  }

  def distance(input: List[String], N: Integer = 40): List[(String, Float)] = {
    if (input.size == 0) return List[(String, Float)]()
    input.foreach(w => {
      if (!contains(w)) {
        println("Out of dictionary word! " + w)
        return List[(String, Float)]()
      }
    })

    val vector = sumVector(input)

    nearestNeighbors(normalize(vector), outSet = input.toSet, N = N)
  }

  /** Find the N closest terms in the vocab to the analogy:
    * - [word1] is to [word2] as [word3] is to ???
    *
    * The algorithm operates as follow:
    * - Find a vector approximation of the missing word = vec([word2]) - vec([word1]) + vec([word3]).
    * - Return words closest to the approximated vector.
    *
    * @param word1 First word in the analogy [word1] is to [word2] as [word3] is to ???.
    * @param word2 Second word in the analogy [word1] is to [word2] as [word3] is to ???
    * @param word3 Third word in the analogy [word1] is to [word2] as [word3] is to ???.
    * @param N The maximum number of terms to return (default to 40).
    *
    * @return The N closest terms in the vocab to the analogy and their associated cosine similarity scores.
    */
  def analogy(word1: String, word2: String, word3: String, N: Integer = 40): List[(String, Float)] = {
    // Check for edge cases
    if (!contains(word1) || !contains(word2) || !contains(word3)) {
      println("Out of dictionary word! " + Array(word1, word2, word3).mkString(" or "))
      return List[(String, Float)]()
    }

    // Find the vector approximation for the missing analogy.
    val vector = new Array[Float](vecSize)
    for (j <- 0 until vector.length)
      vector(j) = vocab.get(word2).get(j) - vocab.get(word1).get(j) + vocab.get(word3).get(j)

    nearestNeighbors(normalize(vector), outSet = Set(word1, word2, word3), N = N)
  }

  /** Rank a set of words by their respective distance to some central term.
    * @param word The central word.
    * @param set Set of words to rank.
    * @return Ordered list of words and their associated scores.
    */
  def rank(word: String, set: Set[String]): List[(String, Float)] = {
    // Check for edge cases
    if (set.size == 0) return List[(String, Float)]()
    (set + word).foreach(w => {
      if (!contains(w)) {
        println("Out of dictionary word! " + w)
        return List[(String, Float)]()
      }
    })

    nearestNeighbors(vocab.get(word).get, inSet = Option(set), N = set.size)
  }

  def pprint(words: List[(String, Float)]) = {
    println("\n%50s".format("Word") + (" " * 7) + "Cosine distance\n" + ("-" * 72))
    println(words.map(s => "%50s".format(s._1) + (" " * 7) + "%15f".format(s._2)).mkString("\n"))
  }
}
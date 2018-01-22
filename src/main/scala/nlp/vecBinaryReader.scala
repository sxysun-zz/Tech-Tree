package main.scala.nlp

import java.io._
import scala.Array
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class VecBinaryReader(val file: File) {

  def this(filename: String) = this(new File(filename))

  private val SPACE = 32
  private val LF = 10

  private val fis = new FileInputStream(file)
  private val bis = new BufferedInputStream(fis)
  private val dis = new DataInputStream(bis)

  def close() { dis.close(); bis.close(); fis.close() }

  def read(): Byte = dis.readByte()

  def readToken(delimiters: Set[Int] = Set(SPACE, LF)): String = {
    val bytes = new ArrayBuffer[Byte]()
    val sb = new StringBuilder()
    var byte = dis.readByte()
    while (!delimiters.contains(byte)) {
      bytes.append(byte)
      byte = dis.readByte()
    }
    sb.append(new String(bytes.toArray[Byte])).toString()
  }

  def readFloat(): Float = {
    // We need to reverse the byte order here due to endian-compatibility.
    java.lang.Float.intBitsToFloat(java.lang.Integer.reverseBytes(dis.readInt()))
  }

}
package main.scala.util

object conversion {
  
  def javaListToScalaList[T](l: java.util.List[T]): List[T] = {
    val terminate = l.size()
    def p(n: Int, nl: List[T]):List[T] = n match {
      case `terminate` => nl
      case _ => {
        p(n+1, nl :+ l.get(n))
      }
    }
    p(0, List())
  }
  
}
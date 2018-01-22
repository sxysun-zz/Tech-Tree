package main.scala.scraper

trait Scraper {
  
  val SEARCHDEPTH: Int
  
  def getContent(): String
  
}
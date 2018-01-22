package main.scala.scraper

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import java.io._

/**
 * @param pageNum this number starts at one
 */
case class googleScholarScraper (searchTerm: String, pageNum: Int) extends Scraper{
  val SEARCHDEPTH: Int = 10
  
  /**
   * @return the URL for this crawler to start its recursion with 
   */
  private val rootURL = JsoupBrowser().get(
      "http://scholar.google.com/scholar?q=" + searchTerm + "&start=" + ((pageNum-1)*10+1)
  )
  
  /**
   * @return a list containing the first value the paper name
   * 	and the second value the paper link
   */
  def getLinksAndPaper(): List[Tuple2[String, String]] = {
    def getLinksTail(html: String, l: List[Tuple2[String, String]], 
        n: Int): List[Tuple2[String, String]] = n match { 
      case 1 => l
      case _ => {
        val startIndex = html.indexOf("gs_rt")
        val endIndex = html.indexOf("data-clk")
        if(startIndex < 0 || endIndex < 0) {
          throw new RuntimeException(s"no links found for the startIndex $startIndex")
        } else {
          try{
            val rawLinks = html.substring(startIndex, endIndex)
            
            println(rawLinks)
            
            val sIndex = rawLinks.indexOf("<a href=\"")
            val eIndex = rawLinks.substring(sIndex+10).indexOf("\"")
            val dsIndex = rawLinks.substring(eIndex+3).indexOf(">")
            val deIndex = rawLinks.substring(dsIndex).indexOf("</a>")
            try {
              getLinksTail(html.substring(endIndex + 20), 
                l :+ Tuple2(rawLinks.substring(dsIndex+2, deIndex).replace("<b>", "").replace("</b>", ""), 
                    rawLinks.substring(sIndex+9, eIndex)),
                n - 1)
            } catch {
              //DEBUG
              case ex: Throwable => {
                getLinksTail(html.substring(endIndex + 20), 
                l :+ Tuple2("",""),
                n - 1)
              }
            }
          } catch {
            case ex : Throwable => {
              getLinksTail(html.substring(endIndex + 20), 
                l :+ Tuple2("",""),
                n - 1)
            }
          }
        }
      }
    }
    val left = this.rootURL.body.innerHtml.indexOf("</style>")
    getLinksTail(this.rootURL.body.innerHtml.substring(left), List(), 10)
  }
  
  /**
   * @return a list containing the first value the professor name
   * 	and the second value the professor link
   */
  def getFamousProfessors() = {
    
  }

  /**
   * @return the raw data
   */
  def getContent(): String = this.pageNum match {
    case SEARCHDEPTH => this.rootURL.body.text
    case _ => 
      this.rootURL.body.text ++ 
        googleScholarScraper(searchTerm, pageNum+1).getContent()
  }
  
}
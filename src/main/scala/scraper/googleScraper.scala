package main.scala.scraper

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import java.net.{HttpURLConnection, SocketTimeoutException, URL}
import scala.collection.JavaConversions._
import java.io._
import java.util.concurrent._
import scala.collection.mutable

/**
 * @param pageNum this number starts at one
 */
case class googleScraper (searchTerm: String, pageNum: Int) extends Scraper{
  val SEARCHDEPTH = 10
  
  //val load = browser.parseFile("core/src/test/resources/example.html")
  private val rootURL = JsoupBrowser().get(
      "http://www.google.com/search?q=" + searchTerm + "&search=" + (pageNum-1)*10
  )
  
  private def extractContentLlinkList(): List[String] = {
    def extractTail(html: String, l: List[String], n: Int): List[String] = n match { 
      case 1 => l
      case _ => {
        val startIndex = html.indexOf("<h3 class=\"r\">")
        val endIndex = html.indexOf("</h3>")
        if(startIndex < 0 || endIndex < 0) {
          throw new RuntimeException(s"no links found for the startIndex $startIndex")
        } else {
          try{
            val rawLinks = html.substring(startIndex, endIndex)
            val sIndex = rawLinks.indexOf("<a href=\"/url?q=")
            val eIndex = rawLinks.substring(sIndex+10).indexOf("\"")
            try {
              extractTail(html.substring(endIndex + 20), 
                l :+ rawLinks.substring(sIndex+9, eIndex),
                n - 1)
            } catch {
              //DEBUG
              case ex: Throwable => {
                extractTail(html.substring(endIndex + 20), 
                l :+ "",
                n - 1)
              }
            }
          } catch {
            case ex : Throwable => {
              extractTail(html.substring(endIndex + 20), 
                l :+ "",
                n - 1)
            }
          }
        }
      }
    }
    extractTail(this.rootURL.body.innerHtml, List(), 10).map(x => {
      try{
        x.substring(x.indexOf("h"), x.indexOf("&amp"))
      } catch {
        case ex : Throwable => {
          "twitter"
        }
      }
    }).filter(x => 
      (!x.contains("twitter") && 
          !x.contains("facebook") && 
          !x.contains("youtube") && 
          !x.contains("play.google") && 
          !x.contains("pdf") && 
          !x.contains("linkedin") && 
          !x.contains("enricoelisi")))
  }
  
  private def extractContent(): String = 
    ("" /: this.extractContentLlinkList().map(x => {
      try{
        JsoupBrowser().get(x).body.text
      } catch {
        case e: Throwable => println(s"exception at $x")
        ""
      }
    })) (_ ++ _)
  
  /**
   * recursive function watch out for stack-overflow
   * @return the raw data scraped from the Internet with given depth
   */
  def getContent(): String = this.pageNum match {
    case SEARCHDEPTH => this.extractContent()
    case _ => this.extractContent() ++ googleScraper(this.searchTerm, this.pageNum+1).getContent()
  }
}
package test.scala.work

import main.scala.scraper.googleScholarScraper

object scholarTest {
  def main(args: Array[String]): Unit = {
    println(googleScholarScraper("scala", 1).getLinksAndPaper())
  }
}
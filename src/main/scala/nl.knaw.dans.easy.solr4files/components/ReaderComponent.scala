package nl.knaw.dans.easy.solr4files.components

import java.net.URL

import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try
import scala.xml.{ Elem, XML }

trait ReaderComponent extends DebugEnhancedLogging {

  val reader: Reader

  trait Reader {
    def loadXml(url: URL): Try[Elem] = {
      getContent(url).flatMap(s => Try(XML.loadString(s)))
    }

    def readLines(url: URL): Try[Seq[String]] = {
      getContent(url).map(_.split("\n"))
    }

    def getContentLength(url: URL): Long = {
      getRawLength(url)
    }.doIfFailure { case e => logger.warn(e.getMessage, e) }
      .getOrElse(-1L)

    def getContent(url: URL): Try[String]

    def getRawLength(url: URL): Try[Long]
  }
}

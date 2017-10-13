package nl.knaw.dans.easy.solr4files.components

import java.net.URL

import nl.knaw.dans.easy.solr4files.HttpStatusException

import scala.util.{ Failure, Success, Try }
import scalaj.http.Http

trait HttpReaderComponent extends ReaderComponent {
  override val reader: Reader = new Reader{
    def getContent(url: URL): Try[String] = {
      Try(Http(url.toString).method("GET").asString).flatMap {
        case response if response.isSuccess => Success(response.body)
        case response => Failure(HttpStatusException(s"getContent($url)", response))
      }
    }

    def getRawLength(url: URL): Try[Long] = {
      Try(Http(url.toString).method("HEAD").asString).flatMap {
        case response if response.isSuccess => Try(response.headers("content-length").toLong)
        case response => Failure(HttpStatusException(s"getSize($url)", response))
      }
    }
  }
}

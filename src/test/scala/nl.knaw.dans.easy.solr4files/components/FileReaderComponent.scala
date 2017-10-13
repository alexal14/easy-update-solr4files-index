package nl.knaw.dans.easy.solr4files.components

import java.io.File
import java.net.{ URL, URLDecoder }

import org.apache.commons.io.FileUtils.readFileToString

import scala.util.Try

trait FileReaderComponent extends ReaderComponent {
  override val reader: Reader = new Reader {
    def getContent(url: URL): Try[String] = {
      val path = URLDecoder.decode(url.getPath, "UTF8")
      Try(readFileToString(new File(path), "UTF8"))
    }

    def getRawLength(url: URL): Try[Long] = {
      Try(new File(url.getPath).length)
    }
  }
}

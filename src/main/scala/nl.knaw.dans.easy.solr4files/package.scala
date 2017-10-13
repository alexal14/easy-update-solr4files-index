/**
 * Copyright (C) 2017 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package nl.knaw.dans.easy

import java.net.URL

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.solr.common.util.NamedList

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{ Failure, Success, Try }
import scala.xml.Node
import scalaj.http.HttpResponse

package object solr4files extends DebugEnhancedLogging {

  type FeedBackMessage = String
  type SolrLiterals = Seq[(String, String)]
  type FileToShaMap = Map[String, String]
  type VocabularyMap = Map[String, String]

  case class HttpStatusException(msg: String, response: HttpResponse[String])
    extends Exception(s"$msg - ${ response.statusLine }, details: ${ response.body }")

  case class SolrStatusException(namedList: NamedList[AnyRef])
    extends Exception(s"solr returned: ${ namedList.asShallowMap().values().toArray().mkString }")

  case class SolrBadRequestException(msg: String, cause: Throwable)
    extends Exception(msg, cause)

  case class SolrDeleteException(query: String, cause: Throwable)
    extends Exception(s"solr delete [$query] failed with ${ cause.getMessage }", cause)

  case class SolrUpdateException(solrId: String, cause: Throwable)
    extends Exception(s"solr update of file $solrId failed with ${ cause.getMessage }", cause)

  case class SolrCommitException(cause: Throwable)
    extends Exception(cause.getMessage, cause)

  case class MixedResultsException[T](results: Seq[T], thrown: Throwable)
  // TODO evolve into candidate for dans.lib.error with takeUntilFailure
    extends Exception(thrown.getMessage, thrown)

  implicit class RichTryStream[T](val left: Seq[Try[T]]) extends AnyVal {

    /** Typical usage: toStream.map(TrySomething).takeUntilFailure */
    def takeUntilFailure: Try[Seq[T]] = {
      val it = left.iterator
      val b = mutable.ListBuffer[T]()

      @tailrec
      def inner(): Try[Seq[T]] = {
        if (!it.hasNext) Success(b)
        else it.next() match {
          case Success(y) =>
            b += y
            inner()
          case Failure(t) => Failure(MixedResultsException(b, t))
        }
      }

      inner()
    }
  }

  abstract sealed class Feedback(val msg: String)
  abstract sealed class FileFeedback(override val msg: String) extends Feedback(msg)
  case class FileSubmittedWithContent(override val msg: String) extends FileFeedback(msg)
  case class FileSubmittedWithJustMetadata(override val msg: String) extends FileFeedback(msg) {
    logger.warn(s"Resubmitted $msg with just metadata")
  }
  case class StoreSubmitted(override val msg: String) extends Feedback(msg)
  case class BagSubmitted(override val msg: String, results: Seq[FileFeedback]) extends Feedback(msg) {
    override def toString: String = {
      val xs = results.groupBy(_.getClass.getSimpleName)
      s"Bag $msg: ${ xs.keySet.map(className => s"${ xs(className).size } times $className").mkString(", ") }"
    }
  }

  val xsiURI = "http://www.w3.org/2001/XMLSchema-instance"

  implicit class RichNode(val left: Node) extends AnyVal {

    def hasType(t: String): Boolean = {
      left.attribute(xsiURI, "type")
        .map(_.text)
        .contains(t)
    }

    def hasNoType: Boolean = {
      left.attribute(xsiURI, "type").isEmpty
    }

    def isStreamingSurrogate: Boolean = {
      left
        .attribute("scheme")
        .map(_.text)
        .contains("STREAMING_SURROGATE_RELATION")
    }

    def isUrl: Boolean = {
      Try(new URL(left.text)).isSuccess
    }
  }

  implicit class TryExtensions2[T](val t: Try[T]) extends AnyVal {
    // copied from easy-bag-store
    // TODO candidate for dans-scala-lib
    def unsafeGetOrThrow: T = {
      t match {
        case Success(value) => value
        case Failure(throwable) => throw throwable
      }
    }
  }
}


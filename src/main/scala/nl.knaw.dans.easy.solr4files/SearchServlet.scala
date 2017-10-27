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
package nl.knaw.dans.easy.solr4files

import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.http.HttpStatus._
import org.apache.solr.client.solrj.SolrQuery
import org.scalatra._

import scala.util.Try
import scalaj.http.HttpResponse

class SearchServlet(app: EasyUpdateSolr4filesIndexApp) extends ScalatraServlet with DebugEnhancedLogging {
  logger.info("File index Servlet running...")

  private def respond(result: Try[String]): ActionResult = {
    val msgPrefix = "Log files should show which actions succeeded. Finally failed with: "
    result.map(Ok(_))
      .doIfFailure { case e => logger.error(e.getMessage, e) }
      .getOrRecover {
        case SolrBadRequestException(message, _) => BadRequest(message) // delete or search only
        case HttpStatusException(message, r: HttpResponse[String]) if r.code == SC_NOT_FOUND => NotFound(message)
        case HttpStatusException(message, r: HttpResponse[String]) if r.code == SC_SERVICE_UNAVAILABLE => ServiceUnavailable(message)
        case HttpStatusException(message, r: HttpResponse[String]) if r.code == SC_REQUEST_TIMEOUT => RequestTimeout(message)
        case e => InternalServerError()
      }
  }

  // injection: https://packetstormsecurity.com/files/144678/apachesolr701-xxe.txt
  // https://lucene.apache.org/solr/guide/6_6/query-syntax-and-parsing.html
  // lucene seems not safe: Support for using any type of query parser as a nested clause
  // edismax: supports the full Lucene query parser syntax, so possibly not safe too
  // dismax: more like [...] Google [...] makes [...] appropriate [...] for many consumer applications
  private val queryParser = "dismax"

  get("/") {
    params.get("text")
      .map(q => respond(app.search(createQuery(q))))
      .getOrElse(BadRequest("filesearch requires param 'text' (a solr dismax query), got " + params.mkString("[",",","]")))
  }

  private def createQuery(query: String) = {
    new SolrQuery() {
      setQuery(query)
      addFilterQuery("easy_file_accessible_to:ANONYMOUS + easy_file_accessible_to:KNOWN")
      // TODO filter query on available date
      setFields("easy_dataset_*", "easy_file_*")
      setStart(0)
      setRows(10)
      setTimeAllowed(5000) // 5 seconds TODO make configurable
      // setFacet... setMoreLikeThis... setHighlight... setDebug... etc

      set("defType", queryParser)
    }
  }
}

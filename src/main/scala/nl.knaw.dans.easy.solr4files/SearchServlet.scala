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

import nl.knaw.dans.easy.solr4files.components.User
import nl.knaw.dans.lib.error._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.http.HttpStatus._
import org.apache.solr.client.solrj.SolrQuery
import org.scalatra._
import org.scalatra.auth.strategy.BasicAuthStrategy.BasicAuthRequest

import scala.util.{ Failure, Success, Try }
import scalaj.http.HttpResponse

class SearchServlet(app: EasyUpdateSolr4filesIndexApp) extends ScalatraServlet with DebugEnhancedLogging {
  logger.info("File index Servlet running...")

  private def respond(result: Try[String]): ActionResult = {
    result.map(Ok(_))
      .doIfFailure { case e => logger.error(e.getMessage, e) }
      .getOrRecover {
        case SolrBadRequestException(message, _) => BadRequest(message) // delete or search only
        case HttpStatusException(message, r: HttpResponse[String]) if r.code == SC_NOT_FOUND => NotFound(message)
        case HttpStatusException(message, r: HttpResponse[String]) if r.code == SC_SERVICE_UNAVAILABLE => ServiceUnavailable(message)
        case HttpStatusException(message, r: HttpResponse[String]) if r.code == SC_REQUEST_TIMEOUT => RequestTimeout(message)
        case _ => InternalServerError()
      }
  }

  // injection: https://packetstormsecurity.com/files/144678/apachesolr701-xxe.txt
  // https://lucene.apache.org/solr/guide/6_6/query-syntax-and-parsing.html
  // lucene seems not safe: Support for using any type of query parser as a nested clause
  // edismax: supports the full Lucene query parser syntax, so possibly not safe too
  // dismax: more like [...] Google [...] makes [...] appropriate [...] for many consumer applications
  private val queryParser = "dismax"

  get("/") {
    // no command line equivalent, use http://localhost:8983/solr/#/fileitems/query
    // or for example:           curl 'http://localhost:8983/solr/fileitems/query?q=*'
    (params.get("text"), app.authenticate(new BasicAuthRequest(request))) match {
      case (None, _) => BadRequest("filesearch requires param 'text' (a solr dismax query), " + params.got)
      case (Some(q), Success(user)) => respond(app.search(createQuery(q, user))) // TODO add message when owner like in the webui?
      case (Some(_), Failure(InvalidCredentialsException(_, _))) => Unauthorized()
      case (Some(_), Failure(AuthorisationNotAvailableException(_))) => ServiceUnavailable("Authentication service not available, try anonymous search")
      case (Some(_), Failure(_)) => InternalServerError()
    }
  }

  private def createQuery(query: String, user: Option[User]) = {
    // invalid optional values are ignored
    val rows = params.get("limit").withFilter(_.matches("[1-9][0-9]*")).map(_.toInt).getOrElse(10)
    val start = params.get("skip").withFilter(_.matches("[0-9]+")).map(_.toInt).getOrElse(0)
    val toAnonymous = "easy_file_accessible_to:ANONYMOUS"
    val toKnown = "easy_file_accessible_to:KNOWN"
    val available = "easy_dataset_date_available:[* TO NOW]"
    new SolrQuery() {
      setQuery(query)
      user match {
        case Some(User(_,_,true,_)) => // archivist: no filters
        case Some(User(_,_,_,true)) => // admin: no filters
          // TODO filterQuery for groups
        case None =>
          addFilterQuery(s"$toAnonymous +$toKnown")
          addFilterQuery(available)
        case Some(User(id,_,_,_)) =>
          // TODO reuse cache of partial filters
          val own = "easy_dataset_depositor_id:" + id
          addFilterQuery(s"$toAnonymous +$toKnown +$own")
          addFilterQuery(s"$available +$own")
      }
      setFields("easy_dataset_*", "easy_file_*") // TODO user configurable like rows and start
      setStart(start)
      setRows(rows) // todo max from application.properties
      setTimeAllowed(5000) // 5 seconds TODO configurable in application.properties
      // setFacet... setMoreLikeThis... setHighlight... setDebug... etc

      set("defType", queryParser)
    }
  }
}

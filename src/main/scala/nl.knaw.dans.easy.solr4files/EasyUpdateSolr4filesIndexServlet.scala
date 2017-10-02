/**
 * Copyright (C) 2015 DANS - Data Archiving and Networked Services (info@dans.knaw.nl)
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
import org.scalatra._

import scala.util.Try

class EasyUpdateSolr4filesIndexServlet(app: EasyUpdateSolr4filesIndexApp) extends ScalatraServlet with DebugEnhancedLogging {
  logger.info("File index Servlet running...")

  get("/") {
    contentType = "text/plain"
    Ok("File index running")
  }

  private def respond(result: Try[String]): ActionResult = {
    result.map(Ok(_))
      .doIfFailure { case e => logger.error(e.getMessage, e) }
      .getOrRecover(_ => InternalServerError())
  }

  post("/update/:store/:uuid") {
    respond(app.update(params("store"),params("uuid")))
  }

  post("/init") {
    params.get("store")
      .map (app.initSingleStore)
      .getOrElse(respond(app.initAllStores()))
  }

  delete("/:store/:uuid") {
    respond(app.delete(s"easy_dataset_id:${params("uuid")}"))
  }

  delete("/:store") {
    respond(app.delete(s"easy_dataset_store_id:${params("store")}"))
  }

  delete("/") {
    params.get("q")
      .map (app.delete)
      .getOrElse(BadRequest(s"delete requires param 'q': a solr query"))
  }
}

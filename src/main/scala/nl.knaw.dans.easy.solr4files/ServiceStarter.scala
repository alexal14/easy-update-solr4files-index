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

import nl.knaw.dans.lib.logging.DebugEnhancedLogging
import org.apache.commons.daemon.{ Daemon, DaemonContext }

class ServiceStarter extends Daemon with DebugEnhancedLogging {
  var app: EasyUpdateSolr4filesIndexApp = _
  var service: EasyUpdateSolr4filesIndexService = _

  override def init(context: DaemonContext): Unit = {
    logger.info("Initializing service...")
    val configuration = Configuration()
    app = new EasyUpdateSolr4filesIndexApp(new ApplicationWiring(configuration))
    service = new EasyUpdateSolr4filesIndexService(configuration.properties.getInt("solr4files.daemon.http.port"), app)
    logger.info("Service initialized.")
  }

  override def start(): Unit = {
    logger.info("Starting service...")
    app.init()
      .flatMap(_ => service.start())
      .unsafeGetOrThrow
    logger.info("Service started.")
  }

  override def stop(): Unit = {
    logger.info("Stopping service...")
    service.stop().unsafeGetOrThrow
  }

  override def destroy(): Unit = {
    service.destroy().unsafeGetOrThrow
    logger.info("Service stopped.")
  }
}

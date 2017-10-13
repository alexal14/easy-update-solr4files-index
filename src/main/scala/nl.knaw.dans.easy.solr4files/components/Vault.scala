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
package nl.knaw.dans.easy.solr4files.components

import java.net.{ URI, URL, URLEncoder }
import java.nio.file.Paths
import java.util.UUID

import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.util.Try

trait Vault extends DebugEnhancedLogging with ReaderComponent {
  val vaultBaseUri: URI

  def getStoreNames: Try[Seq[String]] = for {
    uri <- Try(vaultBaseUri.resolve("stores"))
    _ = logger.info(s"getting storeNames with $uri")
    lines <- reader.readLines(uri.toURL)
  } yield lines.map { line =>
    val trimmed = line.trim.replace("<", "").replace(">", "")
    Paths
      .get(new URI(trimmed).getPath)
      .getFileName.toString
  }

  def getBagIds(storeName: String): Try[Seq[UUID]] = for {
  // no state param (in fact no param at all) so we just get the active bags
    storeURI <- Try(vaultBaseUri.resolve(s"stores/$storeName/bags"))
    lines <- reader.readLines(storeURI.toURL)
  } yield lines
    .withFilter(_.trim.nonEmpty)
    .map(str => UUID.fromString(str.trim))

  def getSize(storeName: String, bagId: UUID, path: String): Long = {
    fileURL(storeName, bagId, path).map(reader.getContentLength).getOrElse(-1L)
  }

  def fileURL(storeName: String, bagId: UUID, file: String): Try[URL] = Try {
    val f = URLEncoder.encode(file, "UTF8")
    vaultBaseUri.resolve(s"stores/$storeName/bags/$bagId/$f").toURL
  }
}

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

import nl.knaw.dans.easy.solr4files.SolrLiterals
import nl.knaw.dans.easy.solr4files.components.FileItem._
import nl.knaw.dans.lib.logging.DebugEnhancedLogging

import scala.xml.Node

case class FileItem(bag: Bag, ddm: DDM, xml: Node) extends DebugEnhancedLogging {

  // see ddm.xsd EasyAccessCategoryType
  private def datasetAccessibleTo = ddm.accessRights match {
    // @formatter:off
    case "OPEN_ACCESS"                      => anonymous
    case "OPEN_ACCESS_FOR_REGISTERED_USERS" => known
    case "GROUP_ACCESS"                     => restrictedGroup
    case "REQUEST_PERMISSION"               => restrictedRequest
    case "NO_ACCESS"                        => none
    case _                                  => none
    // @formatter:off
  }

  val path: String = xml.attribute("filepath").map(_.text).getOrElse("")
  val accessibleTo: String = ( xml \ "accessRights").map(_.text).mkString match {
    case "" => datasetAccessibleTo
    case s => s
  }
  val shouldIndex: Boolean = {
    // without a path we can't create a solrID nor fetch the content
    // multiple or otherwise garbage access rights is treated as "NONE": don't index
    path.nonEmpty && accessible.contains(accessibleTo)
    // TODO drop the next line when the query side implements authorisation
    path.nonEmpty && accessibleTo == anonymous
  }

  val mimeType: String = (xml \ "format").text

  // lazy postpones loading Bag.sha's
  lazy val solrLiterals: SolrLiterals = Seq(
    ("file_path", path),
    ("file_checksum", bag.sha(path)),
    ("file_mime_type", mimeType),
    ("file_accessible_to", accessibleTo)
  )
}

object FileItem {
  private val anonymous = "ANONYMOUS"
  private val known = "KNOWN"
  private val restrictedGroup = "RESTRICTED_GROUP"
  private val restrictedRequest = "RESTRICTED_REQUEST"
  private val none = "NONE"

  /** all above but none */
  private val accessible = Set (anonymous, known, restrictedGroup, restrictedRequest)
}

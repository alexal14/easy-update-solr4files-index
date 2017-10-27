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

import org.apache.http.HttpStatus._
import org.apache.solr.client.solrj.response.QueryResponse
import org.apache.solr.client.solrj.{ SolrClient, SolrRequest, SolrResponse }
import org.apache.solr.common.params.SolrParams
import org.apache.solr.common.util.NamedList
import org.apache.solr.common.{ SolrDocument, SolrDocumentList }
import org.scalamock.scalatest.MockFactory
import org.scalatra.test.scalatest.ScalatraSuite

class SearchServletSpec extends TestSupportFixture
  with ServletFixture
  with ScalatraSuite
  with MockFactory {

  private class StubbedWiring extends ApplicationWiring(createConfig("vault")) {

    override lazy val solrClient: SolrClient = new SolrClient() {
      // can't use mock because SolrClient has a final method

      override def query(params: SolrParams): QueryResponse = mockQueryResponse

      override def close(): Unit = ()

      override def request(solrRequest: SolrRequest[_ <: SolrResponse], s: String): NamedList[AnyRef] =
        throw new Exception("mocked request")
    }

    private def mockQueryResponse = {

      new QueryResponse {
        override def getResults: SolrDocumentList = new SolrDocumentList {
          setNumFound(2)
          setStart(0)
          add(new SolrDocument(new java.util.HashMap[String, AnyRef] {
            put("x", "y")
          }))
          add(new SolrDocument(new java.util.HashMap[String, AnyRef] {
            put("a", "b")
            put("c", "d") // TODO try date once we have a typeHint for the json serialization
          }))
        }
      }
    }
  }

  private val app = new EasyUpdateSolr4filesIndexApp(new StubbedWiring)
  addServlet(new SearchServlet(app), "/*")

  "get /" should "complain about missing argument" in {
    get("/?q=something") {
      body shouldBe "filesearch requires param 'text' (a solr dismax query), got [q -> something]"
      status shouldBe SC_BAD_REQUEST
    }
  }

  it should "return json" in {
    get(s"/?text=nothing") {
      // random order for
      // {"header":{"skip":0,"text":"nothing","found":2,"time_allowed":5000,"limit":10},"fileitems":[[{"x":"y"}],[{"a":"b"},{"c":"d"}]]}
      body should startWith("""{""")
      body should include(""""header":{"""")
      body should include(""""skip":0""")
      body should include(""""text":"nothing"""")
      body should include(""""found":2""")
      body should include(""""time_allowed":5000""")
      body should include(""""limit":10""")
      body should include("""},"""")
      body should include(""""fileitems":[[{"""")
      body should include("""[{"x":"y"}]""")
      body should include("""],[""")
      body should include("""{"a":"b"}""")
      body should include("""},{""")
      body should include("""{"c":"d"}""")
      body should include(""""}]]""")
      body should endWith("""}""")
      status shouldBe SC_OK
    }
  }
}

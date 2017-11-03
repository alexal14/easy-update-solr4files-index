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

import java.net.URI
import java.util
import javax.naming.Context._
import javax.naming._
import javax.naming.ldap.InitialLdapContext

import nl.knaw.dans.easy.solr4files.{ AuthorisationNotAvailableException, InvalidCredentialsException }

import scala.util.{ Failure, Try }

trait LdapAuthenticationComponent extends AuthenticationComponent {
  val usersParentEntry: String
  val ldapProviderUrl: URI
  val initialContextFactory: String = "com.sun.jndi.ldap.LdapCtxFactory"

  trait LdapAuthentication extends Authentication {

    def getUser(userName: String, password: String): Try[User] = Try {
      val ctxt = new InitialLdapContext(ldapEnv(userName, password), null)
      // TODO fetch admin/archivist, group(s) from LDAP (getAtributes("...") from the context?)
      // see (don't use) easy-app/lib-deprecated/dans-ldap/src/main/java/nl/knaw/dans/common/ldap/repo/LdapMapper.java
      User(userName)
    }.recoverWith {
      case t: ServiceUnavailableException =>
        Failure(AuthorisationNotAvailableException(t))
      case t: NoPermissionException =>
        Failure(InvalidCredentialsException(userName, t))
      case t: AuthenticationException =>
        Failure(InvalidCredentialsException(userName, t))
      case t =>
        logger.debug("Unexpected exception", t)
        Failure(new RuntimeException("Error trying to authenticate", t))
    }

    private def ldapEnv(userName: String, password: String) = {
      new util.Hashtable[String, String]() {
        put(PROVIDER_URL, ldapProviderUrl.toASCIIString)
        put(SECURITY_AUTHENTICATION, "simple")
        put(SECURITY_PRINCIPAL, s"uid=$userName, $usersParentEntry")
        put(SECURITY_CREDENTIALS, password)
        put(INITIAL_CONTEXT_FACTORY, initialContextFactory)
      }
    }
  }
}

package org.skrushingiv

import play.api.libs.ws.WSResponse
import play.api.libs.json.{Reads,Writes}
import scala.concurrent._
import scala.util.{Try,Success,Failure}
import play.api.Application
import scala.language.postfixOps
import scala.language.implicitConversions

package object http {

  type PSeq = Seq[(String,String)]

  /**
   * Enables assignment syntax for REST Element reading
   * 
   * for example:
   *     val element:Future[Option[Foo]] = myEndpoint / myId
   */
  implicit def readElement[R <: RESTClient,A](rest:R#RESTElement)(implicit app: Application, r:Reads[A], ec:ExecutionContext):Future[Option[A]] = rest read

  /**
   * Enables assignment syntax for REST collection listing
   * 
   * for example:
   *     val elements:Future[List[Foo]] = myEndpoint / myId / "foo"
   */
  implicit def listCollection[R <: RESTClient,A](rest:R#RESTCollection)(implicit app: Application, r:Reads[A], ec:ExecutionContext):Future[List[A]] = rest list
  
}

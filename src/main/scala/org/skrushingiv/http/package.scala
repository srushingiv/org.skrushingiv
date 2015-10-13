package org.skrushingiv

import play.api.libs.ws.WSResponse
import play.api.libs.json.{Reads,Writes}
import scala.concurrent._
import scala.util.{Try,Success,Failure}

package object http {

  type PSeq = Seq[(String,String)]

  implicit class PimpFutureResponse(val fr: Future[WSResponse]) extends AnyVal {

    def asList[A](implicit r:Reads[A], ec: ExecutionContext):Future[(List[A])] =
      asOpt[List[A]](Reads.list(r),ec) map (_ getOrElse List.empty)

    def asOpt[A](implicit r:Reads[A], ec: ExecutionContext):Future[Option[A]] =
      fr flatMap FTry(_.json.asOpt[A])

    def as[A](implicit r:Reads[A], ec: ExecutionContext):Future[A] =
      fr flatMap FTry(_.json.as[A])
  }

  private def FTry[A](f:WSResponse => A) : WSResponse => Future[A] = wsr =>
    Try(f(wsr)) match {
      case Success(o) => Future.successful(o)
      case Failure(t) => Future.failed(RESTException(t.getMessage(),wsr))
    }

}

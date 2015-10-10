package org.skrushingiv

import play.api.libs.ws.WSResponse
import play.api.libs.json.{Reads,Writes}
import scala.concurrent._
import scala.util.{Try,Success,Failure}

package object http {

  type PSeq = Seq[(String,String)]

  implicit class PimpFutureResponse(val fr: Future[WSResponse]) extends AnyVal {

    def asList[A](implicit r:Reads[A], ec: ExecutionContext):Future[List[A]] =
      asOpt[List[A]](Reads.list(r),ec) map (_ getOrElse List.empty)

    def asOpt[A](implicit r:Reads[A], ec: ExecutionContext):Future[Option[A]] =
      fr map (_.json.asOpt[A])

    def as[A](implicit r:Reads[A], ec: ExecutionContext):Future[A] =
      fr map (_.json.as[A])
  }

}

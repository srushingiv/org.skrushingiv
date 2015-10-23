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
  
}

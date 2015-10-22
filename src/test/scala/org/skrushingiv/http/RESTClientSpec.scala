package org.skrushingiv.http

import org.specs2.mutable._
import org.specs2.specification.Scope
import play.api.libs.json._
import play.api.libs.ws._
import play.api.Application
import scala.util.Try
import scala.concurrent._
import scala.concurrent.duration.Duration

import org.skrushingiv.test._

class RESTClientSpec extends Specification with StrictMocking {
  
  // we just need an implicit pointer in scope. it shouldn't actually get called during testing.
  implicit val app:Application = strictMock[Application]

  // TODO: add tests here

  trait Fixture extends Scope {
    def response:Option[WSResponse] = None
    val myClient = new RESTClient {
      override protected def createRequest(url:String)(implicit app:Application) = MyRequest(url)
      override protected def prepare(req: WSRequest): WSRequest = {
        req.asInstanceOf[MyRequest].copy(returns = response)
      }
    }    
  }

  case class MyRequest(
      url: String,
      method: String = "GET",
      body: WSBody = EmptyBody,
      headers: Map[String, Seq[String]] = Map.empty,
      queryString: Map[String, Seq[String]] = Map.empty,
      calc: Option[WSSignatureCalculator] = None,
      auth: Option[(String, String, WSAuthScheme)] = None,
      followRedirects: Option[Boolean] = None,
      requestTimeout: Option[Int] = None,
      virtualHost: Option[String] = None,
      proxyServer: Option[WSProxyServer] = None,
      returns: Option[WSResponse] = None
  ) extends WSRequest {
    def sign(calc: WSSignatureCalculator) = copy(calc = Some(calc))
    def withAuth(username: String, password: String, scheme: WSAuthScheme) = copy(auth = Some((username,password,scheme)))
    def withHeaders(hdrs: (String, String)*) = {
      val newHdrs = hdrs.groupBy(_._1).map {
        case (k,v) => (k, headers.getOrElse(k, Seq.empty) ++ v.map(_._2))
      }
      copy(headers = headers ++ newHdrs)
    }
    def withQueryString(parameters: (String, String)*) = {
      val newParams = parameters.groupBy(_._1).map {
        case (k,v) => (k, queryString.getOrElse(k, Seq.empty) ++ v.map(_._2))
      }
      copy(queryString = queryString ++ newParams)
    }
    def withFollowRedirects(follow: Boolean) = copy(followRedirects = Some(follow))
    def withRequestTimeout(timeout: Duration) = copy(requestTimeout = Some(timeout.toMillis.toInt))
    def withVirtualHost(vh: String) = copy(virtualHost = Some(vh))
    def withProxyServer(proxyServer: WSProxyServer) = copy(proxyServer = Some(proxyServer))
    def withBody(body: WSBody) = copy(body = body)
    def withMethod(method: String) = copy(method = method)
    def execute(): Future[WSResponse] = {
      returns.map(Future.successful(_)) getOrElse Future.failed(new RuntimeException(""))
    }
    def stream() = ???
  }

  case class MyResponse(
      status: Int,
      statusText: String,
      body: String = "",
      allHeaders: Map[String, Seq[String]] = Map.empty,
      cookies: Seq[WSCookie] = Seq.empty
  ) extends WSResponse {
    def underlying[T]: T = ???
    def header(key: String) = allHeaders.get(key).flatMap(_.headOption)
    def cookie(name: String) = cookies.filter(_.name.map(_ == name) getOrElse false).headOption
    def xml = ???
    def json: JsValue = Json.parse(body)
    def bodyAsBytes = ???
  }
}
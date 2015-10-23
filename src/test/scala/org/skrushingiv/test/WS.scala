package org.skrushingiv.test

import play.api.libs.json._
import play.api.libs.ws._
import play.api.Application
import scala.concurrent._
import scala.util.Try

import _root_.org.skrushingiv.util.TryUtils

object WS {
  
  def mockRequest(url:String, handler:WSRequest => Try[WSResponse]):WSRequest = Request(url, handler)

  case class Request(
      url: String,
      handler: WSRequest => Try[WSResponse],
      method: String = "GET",
      body: WSBody = EmptyBody,
      headers: Map[String, Seq[String]] = Map.empty,
      queryString: Map[String, Seq[String]] = Map.empty,
      calc: Option[WSSignatureCalculator] = None,
      auth: Option[(String, String, WSAuthScheme)] = None,
      followRedirects: Option[Boolean] = None,
      requestTimeout: Option[Int] = None,
      virtualHost: Option[String] = None,
      proxyServer: Option[WSProxyServer] = None
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
    def withRequestTimeout(timeout: Long) = copy(requestTimeout = Some(timeout.toInt))
    def withVirtualHost(vh: String) = copy(virtualHost = Some(vh))
    def withProxyServer(proxyServer: WSProxyServer) = copy(proxyServer = Some(proxyServer))
    def withBody(body: WSBody) = copy(body = body)
    def withMethod(method: String) = copy(method = method)
    def execute(): Future[WSResponse] = handler(this).fold(Future.successful(_),Future.failed(_))
    def stream() = ???
  }

  case class Response(
      body: String = "",
      status: Int = 200,
      statusText: String = "Ok",
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

  object Response {
    def apply(body:String) = new Response(body)
    def apply[A](body:A)(implicit w:Writes[A]) = new Response(Json.toJson(body).toString) 
  }
}
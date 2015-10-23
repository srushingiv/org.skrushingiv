package org.skrushingiv.http

import org.specs2.mutable._
import org.specs2.specification.Scope
import play.api.libs.json._
import play.api.libs.ws.{ WSRequest, WSResponse }
import play.api.Application
import scala.util.{Try, Success, Failure}
import scala.concurrent._
import scala.concurrent.duration.Duration

import org.skrushingiv.util._
import org.skrushingiv.test._

import scala.language.reflectiveCalls
import scala.language.implicitConversions

class RESTClientSpec extends Specification with StrictMocking {
  import ExecutionContext.Implicits.global

  // we just need an implicit pointer in scope. it shouldn't actually get called during testing.
  implicit val app:Application = strictMock[Application]
  implicit val stringReads = Reads.StringReads

  case class Foo(a:String,b:Int)
  implicit val fooFormat = Json.format[Foo]
  val fooList = List(Foo("bar",123), Foo("bar",456))

  "RESTClient" should {
    "generate a valid url for list and parse a list return value" in new Fixture {
      override val handler: Handler = { r => url = r.url; method = r.method; Success(WS.Response(fooList)) }
      val r = foo.list[Foo] get ;
      url === "http://test.com:9000/foo"
      method === "GET"
      r === fooList
    }
    "generate a valid url for create" in new Fixture {
      foo += "Something!" get ;
      url === s"http://test.com:9000/foo"
      method === "POST"
    }
    "generate a valid url for read and parse a single return value" in new Fixture {
      override val handler: Handler = { r => url = r.url; method = r.method; Success(WS.Response(fooList(0))) }
      val r = (foo / id).read[Foo] get ;
      url === s"http://test.com:9000/foo/${id}"
      method === "GET"
      r === Some(fooList(0))
    }
    "generate a valid url for update" in new Fixture {
      foo / id := "SomethingNew!" get ;
      url === s"http://test.com:9000/foo/${id}"
      method === "PUT"
    }
    "generate a valid url for delete" in new Fixture {
      foo -= id get ;
      url === s"http://test.com:9000/foo/${id}"
      method === "DELETE"
    }
    "generate a valid url for action" in new Fixture {
      foo / id > "baz" get ;
      url === s"http://test.com:9000/foo/${id}/baz"
      method === "GET"
    }
    "generate a valid url for deep list and parse a list return value" in new Fixture {
      override val handler: Handler = { r => url = r.url; method = r.method; Success(WS.Response(List(123,456))) }
      val r = (foo / 123 / "bar").list[Int] get ;
      method === "GET"
      url === s"http://test.com:9000/foo/123/bar"
      r === List(123,456)
    }
    "generate a valid url for deep create" in new Fixture {
      foo / 123 / "bar" += "Something!" get ;
      url === s"http://test.com:9000/foo/123/bar"
      method === "POST"
    }
    "generate a valid url for deep read and parse a single return value" in new Fixture {
      override val handler: Handler = { r => url = r.url; method = r.method; Success(WS.Response("\"plugh\"")) }
      val r = (foo / 123 / "bar" / id).read[String] get ;
      url === s"http://test.com:9000/foo/123/bar/${id}"
      method === "GET"
      r === Some("plugh")
    }
    "generate a valid url for deep update" in new Fixture {
      foo / 123 / "bar" / id := "SomethingNew!" get ;
      url === s"http://test.com:9000/foo/123/bar/${id}"
      method === "PUT"
    }
    "generate a valid url for deep delete" in new Fixture {
      foo / 123 / "bar" -= id get ;
      url === s"http://test.com:9000/foo/123/bar/${id}"
      method === "DELETE"
    }
    "generate a valid url for deep action" in new Fixture {
      foo / 123 / "bar" / id > "baz" get ;
      url === s"http://test.com:9000/foo/123/bar/${id}/baz"
      method === "GET"
    }
  }

  trait Fixture extends Scope {
    type Handler = WSRequest => Try[WSResponse]
    val id = math.random
    var url = ""
    var method = ""
    val handler: Handler = { r => url = r.url; method = r.method; Success(WS.Response("")) }
    val test = new RESTClient {
      val rootUrl = "http://test.com:9000"
      override protected def createRequest(url:String)(implicit app:Application) = WS.mockRequest(url, handler)
    }
    val foo = test.CRUDEndpoint("foo")
  }

}
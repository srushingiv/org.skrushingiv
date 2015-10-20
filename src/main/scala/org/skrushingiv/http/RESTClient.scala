package org.skrushingiv.http

import scala.concurrent.{ExecutionContext, Future}
import play.api.http.ContentTypeOf
import play.api.http.Writeable
import play.api.libs.ws.WSResponse
import play.api.libs.json.{Reads,Writes}
import play.api.Application


case class RESTException(msg:String, response:WSResponse) extends RuntimeException(msg)

/**
 * RESTClient endpoints have a very specific CRUD schema. the method name
 * should not appear in the URL, it should be determined based on the HTTP METHOD used.
 * 
 * The schema should be:
 *     endpoint / collection / id / subcollection / id
 * 
 * Error messages should be returned as plain-text strings with an HTTP result code
 * that indicates an error. And that error message will be returned to calling code as
 * a failed future with the message as the exception message.
 * 
 * To get a LIST of items in a collection, use the GET method and the collection url (returns Future[List[Item]]):
 *     GET: endpoint / collection (include search parameters in the query string)
 * 
 * To CREATE a new item, use the POST method and the collection url (returns Future[Unit]):
 *     POST: endpoint / collection (include object JSON in body)
 * 
 * To get a specific item by ID, use the GET method and the item url (returns Future[Option[Item]]):
 *     GET: endpoint / collection / id
 * 
 * To DELETE an item, use the DELETE method and the item url (returns Future[Unit]):
 *     DELETE: endpoint / collection / id
 * 
 * To UPDATE an item, use the PUT method and the item url (returns Future[Option[Item]]):
 *     PUT: endpoint / collection / id (include object JSON in body)
 * 
 * Finally, action methods (or RPC type calls) should be invoked as a final path component
 * after an item:
 *     GET: endpoint / collection / id / action
 * 
 * The same holds true for sub-collection items.
 * 
 * This trait simplifies interaction with REST endpoints by providing the behavior necessary to
 * interact with any endpoint in a consistent way, and by providing a simple means of defining
 * endpoints and their collections.
 * 
 * Intended usage in RESTClient definitions:
 * 
 *     case class MyRESTClient extends RESTClient {
 *       val rootUrl = ...
 *       
 *       val endpoint = CRUDEndpoint("collection")
 *       val more_endpoints ...
 *     }
 * 
 * Intended usage in consuming services:
 * 
 *     def insertNewItem(something:MyClass): Future[Unit] {
 *       myRESTClient.endpoint.create(something)
 *       // generates a POST request to <baseUrl>/collection
 *     }
 * 
 *     def doSomethingWithFoo(myId:UUID,fooId:Int): Future[Unit] {
 *       myRESTClient.endpoint / myId / "foo" / fooId > "do-something"
 *       // generates a GET request to <baseUrl>/collection/<myId>/foo/do-something
 *     }
 */

trait RESTClient extends HttpClient { self =>

  /**
   * This is a convenience method for constructing a REST endpoint url factory.
   */
  protected def mkUrl(pathComponents:Seq[Any]) = pathComponents.map(_.toString) mkString "/"

  protected class RESTItem(id:Any, path: Any*) {
    def apply(subCollection:String) = /(subCollection)
    def /(subCollection:String) = new RESTCollection(subCollection, path :+ id :_*)

    def read[A](implicit app: Application, r:Reads[A], ec:ExecutionContext):Future[Option[A]] = read[A]()(app,r,ec)
    def read[A](params: PSeq = Seq.empty, headers: PSeq = Seq.empty)(implicit app: Application, r:Reads[A], ec:ExecutionContext) =
      get(mkUrl(path :+ id), params, headers).asOpt[A]

    def :=[A](value:A)(implicit app: Application, w:Writes[A], ec:ExecutionContext) = update(value)
    def update[A](value:A, params: PSeq = Seq.empty, headers: PSeq = Seq.empty)(implicit app: Application, w:Writes[A], ec:ExecutionContext) =
      put(mkUrl(path :+ id), w.writes(value), params, headers) map (_ => ())

    def delete(implicit app: Application, ec:ExecutionContext): Future[Unit] = delete()(app,ec)
    def delete(params: PSeq = Seq.empty, headers: PSeq = Seq.empty)(implicit app: Application, ec:ExecutionContext) =
      self.delete(mkUrl(path :+ id), params, headers) map (_ => ())

    def >(name:String)(implicit app:Application, ec:ExecutionContext) = action(name)
    def action(name:String, params: PSeq = Seq.empty, headers: PSeq = Seq.empty)(implicit app: Application, ec:ExecutionContext) =
      get(mkUrl(path :+ id :+ name), params, headers) map (_ => ())
  }

  protected class RESTCollection(name:String, path: Any*) {
    def apply(id:Any) = /(id)
    def /(id:Any) = new RESTItem(id, path :+ name :_*)

    def list[A](implicit app: Application, r:Reads[A], ec:ExecutionContext):Future[List[A]] = list[A]()(app,r,ec)
    def list[A](params: PSeq = Seq.empty, headers: PSeq = Seq.empty)(implicit app: Application, r:Reads[A], ec:ExecutionContext) =
      get(mkUrl(path :+ name), params, headers).asList[A]

    def +[A](value:A)(implicit app: Application, w:Writes[A], ec:ExecutionContext) = create(value)
    def create[A](value:A, params: PSeq = Seq.empty, headers: PSeq = Seq.empty)(implicit app: Application, w:Writes[A], ec:ExecutionContext) =
      post(mkUrl(path :+ name), w.writes(value), params, headers) map (_ => ())

    def -(id:Any)(implicit app: Application, ec:ExecutionContext) = /(id).delete()
  }

  // convenience initializer method which should be used to create CRUD Endpoints.
  final def CRUDEndpoint(path:String) = new RESTCollection(path)

}

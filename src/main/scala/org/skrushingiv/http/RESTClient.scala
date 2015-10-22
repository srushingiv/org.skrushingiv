package org.skrushingiv.http

import scala.concurrent.{ExecutionContext, Future}
import play.api.http.ContentTypeOf
import play.api.http.Writeable
import play.api.libs.ws.WSResponse
import play.api.libs.json.{Reads,Writes}
import play.api.Application
import scala.language.postfixOps

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
  import RESTClient.handleResponse

  /**
   * This method constructs a REST endpoint url from path components.
   */
  protected def mkUrl(pathComponents:Seq[Any]) = pathComponents.map(_.toString) mkString "/"

  class RESTElement(path: Seq[Any]) {
    /**
     * Creates a REST collection endpoint for the specified sub-collection name.
     */
    def /(subCollection:String) =
      // This odd syntax should allow implementations to override the RESTCollection class
      new self.RESTCollection(path :+ subCollection)

    /**
     * Nullary variant of `read` to allow for more succinct syntax when no headers or query parameters are necessary
     */
    def read[A](implicit app: Application, r:Reads[A], ec:ExecutionContext):Future[Option[A]] = read[A]()(app,r,ec)

    /**
     * Reads a REST Element
     */
    def read[A](params: PSeq = Seq.empty, headers: PSeq = Seq.empty)(implicit app: Application, r:Reads[A], ec:ExecutionContext) =
      get(mkUrl(path), params, headers) map handleResponse(_.json.asOpt[A])

    /**
     * Allows infix assignment-style syntax for putting updates to REST elements.
     * 
     * For example:
     *     myEndpoint / myId := newValue
     */
    def :=[A](value:A)(implicit app: Application, w:Writes[A], ec:ExecutionContext) = update(value)
    
    /**
     * Updates an existing REST element with a new value.
     */
    def update[A](value:A, params: PSeq = Seq.empty, headers: PSeq = Seq.empty)(implicit app: Application, w:Writes[A], ec:ExecutionContext) =
      put(mkUrl(path), w.writes(value), params, headers) map handleResponse(_=>())

    /**
     * Deletes a REST element from a collection.
     */
    def delete(params: PSeq = Seq.empty, headers: PSeq = Seq.empty)(implicit app: Application, ec:ExecutionContext) =
      self.delete(mkUrl(path), params, headers) map handleResponse(_=>())

    /**
     * Allows infix operator-style syntax for performing actions on REST elements.
     * 
     * For example:
     *     myEndpoint / myId > "doSomething"
     */
    def >(name:String)(implicit app:Application, ec:ExecutionContext) = action(name)
    
    /**
     * Performs a REST element action.
     */
    def action(name:String, params: PSeq = Seq.empty, headers: PSeq = Seq.empty)(implicit app: Application, ec:ExecutionContext) =
      get(mkUrl(path :+ name), params, headers) map handleResponse(_=>())
  }

  class RESTCollection(path: Seq[Any]) {
    /**
     * Creates a REST element endpoint for the specified id.
     */
    def /(id:Any) =
      // This odd syntax should allow implementations to override the RESTElement class
      new self.RESTElement(path :+ id)

    /**
     * Nullary variant of `list` to allow for more succinct syntax when no headers or query parameters are necessary
     */
    def list[A](implicit app: Application, r:Reads[A], ec:ExecutionContext):Future[List[A]] = list[A]()(app,r,ec)

    /**
     * Gets a list of element members of the REST collection
     */
    def list[A](params: PSeq = Seq.empty, headers: PSeq = Seq.empty)(implicit app: Application, r:Reads[A], ec:ExecutionContext) =
      get(mkUrl(path), params, headers) map handleResponse {
        _.json.asOpt[List[A]](Reads.list(r)) getOrElse List.empty
      }

    /**
     * Allows infix addition-style syntax for creating elements in REST collections.
     * 
     * For example:
     *     myEndpoint / myId / "subCollection" + newValue
     */
    def +[A](value:A)(implicit app: Application, w:Writes[A], ec:ExecutionContext) = create(value)

    /**
     * Posts a new element members to the REST collection
     */
    def create[A](value:A, params: PSeq = Seq.empty, headers: PSeq = Seq.empty)(implicit app: Application, w:Writes[A], ec:ExecutionContext) =
      post(mkUrl(path), w.writes(value), params, headers) map handleResponse(_=>())

    /**
     * Allows infix subtraction-style syntax for deleting elements from REST collections.
     * 
     * For example:
     *     myEndpoint / myId / "subCollection" - mySubId
     */
    def -(id:Any)(implicit app: Application, ec:ExecutionContext) = /(id).delete();
  }

  /**
   * Creates CRUD Endpoints.
   */
  final def CRUDEndpoint(path:String) = new self.RESTCollection(Seq(path))

}

object RESTClient {
  def apply(baseUrl: => String) = new RESTClient {
    def rootUrl = baseUrl
  }
  
  protected def handleResponse[A](f:WSResponse => A): WSResponse => A = { response =>
    if (response.status >= 200 && response.status < 300) f(response)
    else throw RESTException(s"${response.statusText} (${response.status})", response)
  }

}


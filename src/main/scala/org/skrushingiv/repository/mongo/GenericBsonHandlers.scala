package org.skrushingiv.repository.mongo

import reactivemongo.bson._
import shapeless._
import shapeless.labelled.{FieldType, field}

trait DerivedBSONFormats[E] {
  def read(doc: BSONDocument): E
  def write(entity: E): List[Producer[BSONElement]]
}


object DerivedBSONFormats {

  implicit object hnilHandler extends DerivedBSONFormats[HNil] {
    override def read(doc: BSONDocument): HNil = HNil
    override def write(entity: HNil): List[Producer[BSONElement]] = Nil
  }

  implicit def optionalHconsHandler[K <: Symbol, H, T <: HList](implicit r: BSONReader[_ <: BSONValue, H], w: BSONWriter[H, _ <: BSONValue], wk: Witness.Aux[K], tf: Lazy[DerivedBSONFormats[T]]): DerivedBSONFormats[FieldType[K, Option[H]] :: T] = {
    val key = wk.value.name
    new DerivedBSONFormats[FieldType[K, Option[H]] :: T] {
      override def read(doc: BSONDocument) = field[K](doc.getAs[H](key)) :: tf.value.read(doc)
      override def write(hlist: FieldType[K, Option[H]] :: T) = (key → (hlist.head: Option[H])) :: tf.value.write(hlist.tail)
    }
  }

  implicit def hconsHandler[K <: Symbol, H, T <: HList](implicit r: BSONReader[_ <: BSONValue, H], w: BSONWriter[H, _ <: BSONValue], wk: Witness.Aux[K], tf: Lazy[DerivedBSONFormats[T]]): DerivedBSONFormats[FieldType[K, H] :: T] = {
    val key = wk.value.name
    new DerivedBSONFormats[FieldType[K, H] :: T] {
      override def read(doc: BSONDocument) = field[K](doc.getAs[H](key).get) :: tf.value.read(doc)
      override def write(hlist: FieldType[K, H] :: T) = (key → (hlist.head: H)) :: tf.value.write(hlist.tail)
    }
  }

  // NOTE: we use the Class[E] param here in order to allow single 
  implicit def deriveHandler[E, R <: HList](implicit gen: LabelledGeneric.Aux[E, R], f: Lazy[DerivedBSONFormats[R]]): DerivedBSONFormats[E] =
    new DerivedBSONFormats[E] {
      override def read(doc: BSONDocument) = gen.from(f.value.read(doc))
      override def write(entity: E) = f.value.write(gen.to(entity))
    }

}


trait GenericBsonHandlers {

  implicit def deriveHandler[T](implicit derived: Lazy[DerivedBSONFormats[T]]): BSONDocumentHandler[T] = {
    new BSONDocumentReader[T] with BSONDocumentWriter[T] with BSONHandler[BSONDocument, T] {
      override def read(doc: BSONDocument): T = derived.value read doc
      override def write(entity: T): BSONDocument = document(derived.value write entity : _*)
    }
  }

}

object GenericBsonHandlers extends GenericBsonHandlers

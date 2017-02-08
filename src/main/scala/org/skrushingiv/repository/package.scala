package org.skrushingiv

import shapeless.tag
import shapeless.tag._

import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.language.implicitConversions

/**
 * This package represents my personal approach to the Repository Pattern. In
 * addition to the basics, it attempts to decouple the domain query object and
 * re-use base repository code by applying the Typeclass Pattern for querying
 * the repository.
 * 
 * In essence, this allows simplified replacement of the underlying storage
 * (via the Repository Pattern) combined with simplified extension of the
 * querying system into the repository (via the Typeclass Pattern).
 * 
 * This package also attempts to address the anti-pattern of repeating data
 * structures for entities with and without IDs.
 */
package object repository {

  /**
   * The id is tagged with the Entity type so that the compiler will
   * be able to prevent cross-use of IDs between disparate entity types that
   * share the same ID type but not necessarily the same ID space.
   */
  type EntityId[E <: Entity] = E#Id @@ E

  /**
   * In order to keep from having to provide alternative structures for entities
   * with IDs vs Entities without IDs, we'll use a Tuple of IDs paired with Entity
   * data.
   */
  type IdEntityPair[E <: Entity] = (EntityId[E], E)

  /**
   * Allows us to treat IdEntityPairs as bare Entities by implicitly unwrapping them.
   */
  implicit def toEntity[E <: Entity](pair: IdEntityPair[E]): E = pair._2

  /**
   * Provides a human-readable getter function for the ID value in an IdEntityPair.
   * 
   * NOTE: If the Entity already has a property or method named "id" then that
   * will shadow this functionality.
   */
  implicit class EnrichedIdEntity[E <: Entity](val pair: IdEntityPair[E]) extends AnyVal {
    def id: EntityId[E] = pair._1
  }

  // ------- Play JSON Combinators for Entities --------
  // This allows for serialization of entities to external systems / transports

  /**
   * Creates an Entity ID reader when no explicit reader has been defined.
   */
  implicit def idReads[E <: Entity](implicit r: Reads[E#Id]): Reads[EntityId[E]] =
    (__ \ "id").read[E#Id].map(tag[E].apply)

  /**
   * Derives an id-entity pair reader from readers for the id and entity.
   */
  implicit def idEntityPairReads[E <: Entity](implicit idR: Reads[EntityId[E]], r: Reads[E]) =
    (idR ~ r)(_ -> _)

  /**
   * Creates an Entity ID writer when no explicit writer has been defined.
   */
  implicit def idWrites[E <: Entity](implicit w: Writes[E#Id]): OWrites[EntityId[E]] =
    (__ \ "id").write[E#Id]

  /**
   * Derives an id-entity pair writer from writers for the id and entity.
   */
  implicit def idEntityPairWrites[E <: Entity](implicit idW: OWrites[EntityId[E]], w: OWrites[E]) =
    (idW ~ w)(identity[IdEntityPair[E]] _)

}

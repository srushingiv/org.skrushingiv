package org.skrushingiv.repository

import shapeless.tag
import scala.util.{Try, Success}

/**
 * The base trait for all entities.
 * 
 * The idea behind this Entity trait is that implementations will most
 * often be case classes, and that these classes will know the type of
 * their primary identifier, even if they don't contain one yet.
 * 
 * The entity itself may not include the actual ID, as it may not yet
 * have been persisted or prepared for persisting.
 */
trait Entity extends Product with Serializable {
  // extending `Product` and `Serializable` encourages the use of `case class`

  /**
   * The type that IDs must conform to in order to be paired with this Entity class.
   */
  type Id
}


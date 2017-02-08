package org.skrushingiv.repository

import play.api.libs.json._
import shapeless.tag

case class TestEnt(foo: String, bar: Boolean) extends Entity { type Id = Int }

object TestEnt {
  implicit val format = Json.format[TestEnt]
}

object Main extends App {
  val e: IdEntityPair[TestEnt] = tag[TestEnt](123) -> TestEnt("foo", false)
  
  val intReads:Reads[Int] = implicitly
  
  val testIdReads: Reads[EntityId[TestEnt]] = implicitly // idReads[TestEnt]
  
  
  val pairReads: Reads[IdEntityPair[TestEnt]] = implicitly
  
  
  e.id
}
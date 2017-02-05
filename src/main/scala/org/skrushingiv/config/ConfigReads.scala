package org.skrushingiv.config

import com.typesafe.config.{Config, ConfigException, ConfigObject, ConfigValue, ConfigValueType, ConfigList}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Buffer
import scala.collection.JavaConverters._
import scala.language.higherKinds

abstract class ConfigReads[T](val valueType: ConfigValueType = ConfigValueType.OBJECT) {

  def read(value: ConfigValue): Option[T]

  private[config] def option(v: ConfigValue): Option[T] = if (v.valueType == valueType) read(v) else None

  def map[O](f: T ⇒ O) = {
    val self = this
    new ConfigReads[O](valueType) {
      def read(value: ConfigValue): Option[O] = self.read(value).map(f)
    }
  }

  def flatMap[O](f: T ⇒ Option[O]) = {
    val self = this
    new ConfigReads[O](valueType) {
      def read(value: ConfigValue): Option[O] = self.read(value).flatMap(f)
    }
  }
}

object ConfigReads {
  // Low-priority pre-defined readers for config values

  implicit val configValueReads = new ConfigReads[ConfigValue](ConfigValueType.NULL) {
    def read(value: ConfigValue) = Some(value)
    override private[config] def option(v: ConfigValue): Option[ConfigValue] = read(v)
  }

  implicit val nullValueReads = new ConfigReads[Null](ConfigValueType.NULL) {
    def read(value: ConfigValue) = Some(null)
  }

  implicit val booleanValueReads = new ConfigReads[Boolean](ConfigValueType.BOOLEAN) {
    def read(value: ConfigValue) = Some(value.unwrapped.asInstanceOf[java.lang.Boolean])
  }

  implicit val intValueReads = new ConfigReads[Int](ConfigValueType.NUMBER) {
    def read(value: ConfigValue) = Some(value.unwrapped.asInstanceOf[java.lang.Number].intValue)
  }

  implicit val longValueReads = new ConfigReads[Long](ConfigValueType.NUMBER) {
    def read(value: ConfigValue) = Some(value.unwrapped.asInstanceOf[java.lang.Number].longValue)
  }

  implicit val doubleValueReads = new ConfigReads[Double](ConfigValueType.NUMBER) {
    def read(value: ConfigValue) = Some(value.unwrapped.asInstanceOf[java.lang.Number].doubleValue)
  }

  implicit val stringValueReads = new ConfigReads[String](ConfigValueType.STRING) {
    def read(value: ConfigValue) = Some(value.unwrapped.asInstanceOf[String])
  }

  implicit def listValueReads[T, L[_]](implicit cv: ConfigReads[T], cbf: CanBuildFrom[L[_], T, L[T]]) = new ConfigReads[L[T]](ConfigValueType.LIST) {
    def read(value: ConfigValue) = Some {
      value.asInstanceOf[ConfigList].asScala.foldLeft(cbf()) {
        case (builder, v) ⇒ cv.read(v) match {
          case Some(t) ⇒ builder += t
          case _       ⇒ builder
        }
      }.result
    }
  }

  implicit def mapValueReads[T](implicit cv: ConfigReads[T]) = new ConfigReads[Map[String, T]] {
    def read(value: ConfigValue) = Some(value.asInstanceOf[ConfigObject].unwrapped.asScala.map {
      case (k, v) ⇒ cv.read(v.asInstanceOf[ConfigValue]).map(k → _)
    }.flatten.toMap)
  }
}

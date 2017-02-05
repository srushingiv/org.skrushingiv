package org.skrushingiv

import com.typesafe.config.{Config, ConfigObject, ConfigValue, ConfigValueType, impl}

package object config {

  implicit class EnrichedConfigValue(val value: ConfigValue) extends AnyVal {

    def get(keys: java.util.Iterator[String]): Option[ConfigValue] = {
      if (!keys.hasNext) Some(value)
      else if (value.valueType != ConfigValueType.OBJECT) None
      else Option(value.asInstanceOf[ConfigObject].get(keys.next)) flatMap (_.get(keys))
    }

    def get(keys: java.util.List[String]): Option[ConfigValue] = get(keys.iterator)

    def get(path: String): Option[ConfigValue] = get(impl.ConfigImplUtil.splitPath(path))
  }

  implicit class EnrichedConfig(val config: Config) extends AnyVal {

    def get(path: String): Option[ConfigValue] = (config.root: ConfigValue) get path

  }

  implicit class EnrichedConfigValueOption(val valueOpt: Option[ConfigValue]) extends AnyVal {

    def as[T](implicit reads: ConfigReads[T]): Option[T] = valueOpt flatMap reads.option

    def isNull: Boolean = as[Null].isDefined

  }
}

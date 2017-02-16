package org.skrushingiv.repository.query

sealed trait SortDir
case object Asc extends SortDir { override def toString = SortDir.ASC }
case object Desc extends SortDir { override def toString = SortDir.DESC }

object SortDir {
  private[query] val ASC = "asc"
  private[query] val DESC = "desc"

  def unapply(name: String): Option[SortDir] = name.toLowerCase match {
    case this.ASC => Some(Asc)
    case this.DESC => Some(Desc)
    case _ => None
  }
}

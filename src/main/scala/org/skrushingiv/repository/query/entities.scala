package org.skrushingiv.repository.query

case class Pagination(page: Int, limit: Int) {
  require(page > 0, "page must be greater than 0")
  require(limit > 0, "limit must be greater than 0")
}

object Pagination {
  val default = Pagination(1, 25)
}

case class Sortable(fieldName: String, dir: SortDir = Asc)

package org.skrushingiv.repository

trait AllQueriesFromInternal { self: Repository =>

  protected val allQuery: InternalQuery

  def findAll = find(allQuery, None, None)

  def removeAll = remove(allQuery)

  def countAll = count(allQuery)

}

trait AllQueriesFromDomain { self: Repository =>

  protected val allQuery: Query

  def findAll = findBy(allQuery)

  def removeAll = removeBy(allQuery)

  def countAll = countBy(allQuery)

}

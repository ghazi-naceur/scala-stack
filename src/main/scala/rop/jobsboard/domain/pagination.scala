package rop.jobsboard.domain

object pagination {
  final case class Pagination(limit: Int, offset: Int)
  object Pagination {

    val defaultPageSize = 20
    def apply(potentialLimit: Option[Int], potentialOffset: Option[Int]) =
      new Pagination(potentialLimit.getOrElse(defaultPageSize), potentialOffset.getOrElse(0))

    def default = new Pagination(defaultPageSize, 0)
  }
}

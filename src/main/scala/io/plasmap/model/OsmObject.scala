package io.plasmap.model

import io.plasmap.model.geometry.Point

sealed trait OsmObject{

  def id: OsmId
  def user: Option[OsmUser]
  def version: OsmVersion

  def tags:List[OsmTag]

  def tagsToString = tags.mkString("[",",","]")

  def isNode = this match {
    case _:OsmNode      => true
    case _:OsmWay       => false
    case _:OsmRelation  => false
  }

  def isWay = this match {
    case _:OsmNode      => false
    case _:OsmWay       => true
    case _:OsmRelation  => false
  }

  def isRelation = this match {
    case _:OsmNode      => false
    case _:OsmWay       => false
    case _:OsmRelation  => true
  }

  def nodeOption: Option[OsmNode] = this match {
    case n: OsmNode     => Some(n)
    case _: OsmWay      => None
    case _: OsmRelation => None
  }

  def wayOption: Option[OsmWay] = this match {
    case _: OsmNode     => None
    case w: OsmWay      => Some(w)
    case _: OsmRelation => None
  }

  def relationOption: Option[OsmRelation] = this match {
    case _: OsmNode     => None
    case _: OsmWay      => None
    case r: OsmRelation => Some(r)
  }

  def fold[A](
               node     :OsmNode     => A,
               way      :OsmWay      => A,
               relation :OsmRelation => A) = {
    this match {
      case n:OsmNode      => node(n)
      case w:OsmWay       => way(w)
      case r:OsmRelation  => relation(r)
    }
  }

}
case class OsmNode(id: OsmId, user: Option[OsmUser] = None, version:OsmVersion = OsmVersion(), tags: List[OsmTag], point : Point) extends OsmObject{
  override def toString() = {
    StringBuilder.newBuilder.++=(id.toString).++=(",").++=(tagsToString).++=(",").++=(point.toString).toString()
  }
}
case class OsmWay(id: OsmId, user: Option[OsmUser] = None, version:OsmVersion = OsmVersion(), tags : List[OsmTag], nds : List[OsmId]) extends OsmObject{
  override def toString() = {
    StringBuilder.newBuilder.++=(id.toString).++=(",").++=(tagsToString).++=(",").++=(nds.mkString("[",",","]")).toString()
  }
}
case class OsmRelation(id: OsmId, user: Option[OsmUser] = None, version:OsmVersion = OsmVersion(), tags : List[OsmTag], refs : List[OsmMember]) extends OsmObject {
 override def toString() = {
   StringBuilder.newBuilder.++=(id.toString).++=(",").++=(tagsToString).++=(",").++=(refs.mkString("[",",","]")).toString()
 }
}
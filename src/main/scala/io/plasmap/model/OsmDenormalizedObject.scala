package io.plasmap.model

import io.plasmap.serializer.GeoJsonSerialiser
import io.plasmap.model.geometry._
import io.plasmap.serializer.GeoJsonSerialiser

sealed trait OsmDenormalizedObject{

  def id: OsmId
  def user: Option[OsmUser]
  def version: OsmVersion
  def tags:List[OsmTag]
  type T  <: Geometry
  def geometry:T

  def toGeoJson(withTags:Boolean):String = {
    val properties:Map[String, String] = if(withTags) tags.map(x ⇒ x.key → x.value).toMap else Map()
    GeoJsonSerialiser.jsonFromFeature(Feature(geometry, properties))
  }

  override def toString = toGeoJson(withTags = false)

  def isNode = this match {
    case _:OsmDenormalizedNode      => true
    case _:OsmDenormalizedWay       => false
    case _:OsmDenormalizedRelation  => false
  }

  def isWay = this match {
    case _:OsmDenormalizedNode      => false
    case _:OsmDenormalizedWay       => true
    case _:OsmDenormalizedRelation  => false
  }

  def isRelation = this match {
    case _:OsmDenormalizedNode      => false
    case _:OsmDenormalizedWay       => false
    case _:OsmDenormalizedRelation  => true
  }

  def nodeOption:Option[OsmDenormalizedNode] = this match {
    case n:OsmDenormalizedNode      => Some(n)
    case _:OsmDenormalizedWay       => None
    case _:OsmDenormalizedRelation  => None
  }

  def wayOption:Option[OsmDenormalizedWay] = this match {
    case _:OsmDenormalizedNode      => None
    case w:OsmDenormalizedWay       => Some(w)
    case _:OsmDenormalizedRelation  => None
  }

  def relationOption:Option[OsmDenormalizedRelation] = this match {
    case _:OsmDenormalizedNode      => None
    case _:OsmDenormalizedWay       => None
    case r:OsmDenormalizedRelation  => Some(r)
  }

  def fold[A](
               node     :OsmDenormalizedNode     => A,
               way      :OsmDenormalizedWay      => A,
               relation :OsmDenormalizedRelation => A) = {
    this match {
      case n:OsmDenormalizedNode      => node(n)
      case w:OsmDenormalizedWay       => way(w)
      case r:OsmDenormalizedRelation  => relation(r)
    }
  }

}

case class OsmDenormalizedNode(id: OsmId, user: Option[OsmUser] = None, version:OsmVersion = OsmVersion(), tags : List[OsmTag], geometry : Point) extends OsmDenormalizedObject{
  type T = Point
}
case class OsmDenormalizedWay(id: OsmId, user: Option[OsmUser] = None, version:OsmVersion = OsmVersion(), tags : List[OsmTag], geometry : LineString) extends OsmDenormalizedObject{
  type T = LineString
}
case class OsmDenormalizedRelation(id: OsmId, user: Option[OsmUser] = None, version:OsmVersion = OsmVersion(), tags : List[OsmTag], geometry : GeometryCollection) extends OsmDenormalizedObject{
  type T = GeometryCollection
}
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
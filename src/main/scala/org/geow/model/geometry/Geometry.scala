package org.geow.model.geometry

import org.geow.geohash.{GeoHash, PrecisionUltraHigh_1MM}
import org.geow.serializer.GeoJsonSerialiser


sealed trait Geometry { override def toString = GeoJsonSerialiser.jsonFromGeometry(this) }

sealed trait Point extends Geometry {def lon:Double; def lat:Double}
final case class LonLatPoint(lon:Double, lat:Double) extends Point
final case class HashPoint(hash:Long) extends Point {
  def lon = Point.hashPointToLonLatPoint(this).lon
  def lat = Point.hashPointToLonLatPoint(this).lat
}
final case class MultiPoint(coordinates:List[(Double, Double)]) extends Geometry
final case class LineString(coordinates:List[(Double, Double)]) extends Geometry
final case class MultiLineString(coordinates:List[List[(Double, Double)]]) extends Geometry
final case class Polygon(coordinates:List[List[(Double, Double)]]) extends Geometry
final case class MultiPolygon(coordinates:List[List[List[(Double, Double)]]]) extends Geometry
final case class GeometryCollection(geometries:List[Geometry]) extends Geometry

final case class Feature(geometry:Geometry, properties:Map[String, String])

final case class FeatureCollection(features:List[Feature])

//Points are special.
object Point {
  def apply(lon:Double, lat:Double) = LonLatPoint(lon, lat)
  def apply(hash:Long) = HashPoint(hash)

  lazy val hashCoder = GeoHash(PrecisionUltraHigh_1MM)

  implicit def hashPointToLonLatPoint(hashPoint: HashPoint):LonLatPoint = {
    val (lon, lat) = hashCoder.decodeParallel(hashPoint.hash)
    LonLatPoint(lon, lat)
  }

  implicit def lonLatPointToHashPoint(lonLatPoint: LonLatPoint):HashPoint =
    HashPoint(hashCoder.encodeParallel(lonLatPoint.lon, lonLatPoint.lat))
}



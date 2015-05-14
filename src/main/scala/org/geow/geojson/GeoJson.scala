package org.geow.geojson

import org.geow.geohash.{PrecisionUltraHigh_1MM, GeoHash}
import org.geow.serializer.GeoJsonSerialiser

/**
 * Created by mark on 12.05.15.
 */
//sealed trait Geometry { override def toString = GeoJsonSerialiser.jsonFromGeometry(this) }
//
//sealed trait Point extends Geometry
//final case class HashPoint(hash:Long) extends Point
//final case class LonLatPoint(lon:Double, lat:Double) extends Point
//final case class MultiPoint(coordinates:List[Point]) extends Geometry
//final case class LineString(coordinates:List[Point]) extends Geometry
//final case class MultiLineString(coordinates:List[List[Point]]) extends Geometry
//final case class Polygon(coordinates:List[List[Point]]) extends Geometry
//final case class MultiPolygon(coordinates:List[List[List[Point]]]) extends Geometry
//final case class GeometryCollection(geometries:List[Geometry]) extends Geometry
//
//final case class Feature(geometry:Geometry, properties:Map[String, String])
//
//final case class FeatureCollection(features:List[Feature])
//
////Points are special.
//object Point {
//  def apply(lon:Double, lat:Double) = LonLatPoint(lon, lat)
//  def apply(hash:Long) = HashPoint(hash)
//
//  lazy val hashCoder = GeoHash(PrecisionUltraHigh_1MM)
//
//  implicit def hashPointToLonLatPoint(hashPoint: HashPoint):LonLatPoint = {
//    val (lon, lat) = hashCoder.decodeParallel(hashPoint.hash)
//    LonLatPoint(lon, lat)
//  }
//
//  implicit def lonLatPointToHashPoint(lonLatPoint: LonLatPoint):HashPoint =
//    HashPoint(hashCoder.encodeParallel(lonLatPoint.lon, lonLatPoint.lat))
//}

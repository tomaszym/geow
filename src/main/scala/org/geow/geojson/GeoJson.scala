package org.geow.geojson

import org.geow.geojson.GeoJson.LonLat

/**
 * Created by mark on 12.05.15.
 */
object GeoJson {
  type LonLat = (Double, Double)
}

sealed trait Geometry

final case class Point(coordinates:LonLat) extends Geometry
final case class MultiPoint(coordinates:List[LonLat]) extends Geometry
final case class LineString(coordinates:List[LonLat]) extends Geometry
final case class MultiLineString(coordinates:List[List[LonLat]]) extends Geometry
final case class Polygon(coordinates:List[List[LonLat]]) extends Geometry
final case class MultiPolygon(coordinates:List[List[List[LonLat]]]) extends Geometry
final case class GeometryCollection(geometries:List[Geometry]) extends Geometry

final case class Feature(geometry:Geometry, properties:Map[String, String])

final case class FeatureCollection(features:List[Feature])

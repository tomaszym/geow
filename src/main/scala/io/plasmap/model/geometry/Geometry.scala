package io.plasmap.model.geometry

import io.plasmap.geohash.PrecisionUltraHigh_1MM
import io.plasmap.geohash.{GeoHash, PrecisionUltraHigh_1MM}
import io.plasmap.serializer.GeoJsonSerialiser

import scala.annotation.tailrec


sealed trait Geometry { override def toString = GeoJsonSerialiser.jsonFromGeometry(this) }

sealed trait Point extends Geometry {def lon:Double; def lat:Double; def hash:Long}

final case class LonLatPoint(lon:Double, lat:Double) extends Point {
  def hash = Point.lonLatPointToHashPoint(this).hash
}
final case class HashPoint(hash:Long) extends Point {
  def lon = Point.hashPointToLonLatPoint(this).lon
  def lat = Point.hashPointToLonLatPoint(this).lat
}
final case class MultiPoint(coordinates:List[(Double, Double)]) extends Geometry
final case class LineString(coordinates:List[(Double, Double)]) extends Geometry
final case class MultiLineString(coordinates:List[List[(Double, Double)]]) extends Geometry
final case class Polygon(coordinates:List[List[(Double, Double)]]) extends Geometry
final case class MultiPolygon(coordinates:List[List[List[(Double, Double)]]]) extends Geometry

final case class GeometryCollection(geometries:List[Geometry]) extends Geometry {
  def flatten:List[Geometry] = { //DFS through the geometries
    @tailrec def go(done:List[Geometry], todo:List[Geometry]):List[Geometry] = {
      if(todo == Nil) done
      else{
        todo.head match {
          case t:GeometryCollection ⇒ go(     done , t.geometries ++ todo.tail)
          case x                    ⇒ go(x :: done ,                 todo.tail)
        }
      }
    }
    go(Nil, geometries)
  }
}

final case class Feature(geometry:Geometry, properties:Map[String, String])

final case class FeatureCollection(features:List[Feature])

//Points are special.
object Point {
  def apply(lon:Double, lat:Double) = LonLatPoint(lon, lat)
  def apply(hash:Long) = HashPoint(hash)

  lazy val hashCoder = GeoHash(PrecisionUltraHigh_1MM)

  def hashPointToLonLatPoint(hashPoint: HashPoint):LonLatPoint = {
    val (lon, lat) = hashCoder.decodeParallel(hashPoint.hash)
    LonLatPoint(lon, lat)
  }

  def lonLatPointToHashPoint(lonLatPoint: LonLatPoint):HashPoint =
    HashPoint(hashCoder.encodeParallel(lonLatPoint.lon, lonLatPoint.lat))
}



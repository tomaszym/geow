package org.geow.parser.impl

import org.geow.model.OsmDenormalizedObject
import org.geow.model.geometry._
import org.geow.parser.OsmDenormalizedParser
import org.geow.serializer.OsmDenormalisedGeoJSONBijections._
import org.geow.serializer.GeoJsonSerialiser._
import argonaut._, Argonaut._, Shapeless._

/**
 * Created by mark on 14.05.15.
 */
final case class Boundary(id:Int,name:String,localname:String,SRID:Int, adminLevel:Int, tags:Map[String, String], geometry:Geometry)
final case class Boundaries(boundaries:List[Boundary])

class OsmBoundaryParser(json:String) extends OsmDenormalizedParser {

  implicitly[DecodeJson[List[List[List[Point]]]]]

  implicit def BoundaryDecodeJson:DecodeJson[Boundary]  = DecodeJson( c ⇒ for {
    id ← (c --\ "id").as[Int]
    name ← (c --\ "name").as[String]
    localname ← (c --\ "localname").as[String]
    srid ← (c --\ "SRID").as[Int]
    adminLevel ← (c --\ "adminLevel").as[Int]
    tags ← (c --\ "tags").as[Option[Map[String, String]]]
    geometry ← (c --\ "coordinates").as[List[List[List[(Double, Double)]]]].map[MultiPolygon](MultiPolygon)
  } yield Boundary(id, name, localname, srid, adminLevel, tags.getOrElse(Map()), geometry))

  implicit def BoundariesDecodeJson:DecodeJson[Boundaries] = jdecode1L(Boundaries.apply)("boundaries")

  private def boundaryToFeature(b:Boundary):Feature = {
    import OsmPropertyNames._
    Feature(b.geometry, b.tags ++ Map(
      osmId → b.id.toString,
      osmVersionChangeset → 1.toString,
      osmVersionNumber → 1.toString,
      osmVersionVisible → true.toString,
      osmVersionTimestamp → 0.toString,
      osmUserId → "None",
      osmUserName → "None",
      "name" → b.name,
      "local_name" → b.localname,
      "admin_level" → b.adminLevel.toString
    )
    )
  }

  private val iterator = json.decodeOption[Boundaries]
    .getOrElse(Boundaries(Nil))
    .boundaries
    .iterator

  override def hasNext: Boolean = iterator.hasNext
  override def next(): Option[OsmDenormalizedObject] = geoJsonToDenormalized(boundaryToFeature(iterator.next()))
}

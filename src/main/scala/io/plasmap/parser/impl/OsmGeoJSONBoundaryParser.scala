package io.plasmap.parser.impl

import io.plasmap.serializer.OsmDenormalisedGeoJSONBijections.OsmPropertyNames
import io.plasmap.model.OsmDenormalizedObject
import io.plasmap.model.geometry._
import io.plasmap.parser.OsmDenormalizedParser
import io.plasmap.serializer.OsmDenormalisedGeoJSONBijections._
import argonaut._, Argonaut._, Shapeless._

import scala.io.{Codec, Source}

/**
 * Created by mark on 14.05.15.
 */
final case class Boundary(id:Int,name:String,localname:String,SRID:String, adminLevel:Int, tags:Map[String, String], geometry:GeometryCollection)
final case class Boundaries(boundaries:List[Boundary])

case class OsmGeoJSONBoundaryParser(source:Source) extends OsmDenormalizedParser {
  import OsmGeoJSONBoundaryParser._

  val json = source.mkString

  private val iterator = json.decodeOption[Boundaries]
    .getOrElse(Boundaries(Nil))
    .boundaries
    .iterator

  override def hasNext: Boolean = iterator.hasNext
  override def next(): Option[OsmDenormalizedObject] = geoJsonToDenormalized(boundaryToFeature(iterator.next()))
}

object OsmGeoJSONBoundaryParser {
  def apply(fileName: String)(implicit codec:Codec): OsmGeoJSONBoundaryParser = OsmGeoJSONBoundaryParser(Source.fromFile(fileName)(codec))
  implicitly[DecodeJson[List[List[List[Point]]]]]

  def getGeometryFromType(typ:String, c:HCursor):DecodeResult[GeometryCollection] = {
    val a: ACursor = c --\ "coordinates"
    typ match {
      case "MultiPolygon" ⇒ a.as[List[List[List[(Double, Double)]]]].map[MultiPolygon](MultiPolygon).map[GeometryCollection](mp ⇒ GeometryCollection(List(mp)))
      case "LineString" ⇒ a.as[List[(Double, Double)]].map[LineString](LineString).map[GeometryCollection](ls ⇒ GeometryCollection(List(ls)))
    }
  }

  implicit def BoundaryDecodeJson:DecodeJson[Boundary]  = DecodeJson( c ⇒ for {
    id ← (c --\ "id").as[Int]
    name ← (c --\ "name").as[String]
    localname ← (c --\ "localname").as[String]
    srid ← (c --\ "SRID").as[String]
    adminLevel ← (c --\ "admin_level").as[Int]
    tags ← (c --\ "tags").as[Option[Map[String, String]]]
    typ ← (c --\ "type").as[String]
    geometry ← getGeometryFromType(typ, c)
  } yield Boundary(id, name, localname, srid, adminLevel, tags.getOrElse(Map()), geometry))

  implicit def BoundariesDecodeJson:DecodeJson[Boundaries] = jdecode1L(Boundaries.apply)("boundaries")

  def boundaryToFeature(b:Boundary):Feature = {
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
}

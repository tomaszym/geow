package io.plasmap.parser.impl

import io.circe.Decoder.Result
import io.plasmap.serializer.OsmDenormalisedGeoJSONBijections.OsmPropertyNames
import io.plasmap.model.OsmDenormalizedObject
import io.plasmap.model.geometry._
import io.plasmap.parser.OsmDenormalizedParser
import io.plasmap.serializer.OsmDenormalisedGeoJSONBijections._

import scala.io.{Codec, Source}
import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._


final case class Boundary(id:Int,name:String,localname:String,SRID:String, adminLevel:Int, tags:Map[String, String], geometry:GeometryCollection)
final case class Boundaries(boundaries:List[Boundary])

case class OsmGeoJSONBoundaryParser(source:Source) extends OsmDenormalizedParser {
  import OsmGeoJSONBoundaryParser._

  val json = source.mkString

  private val iterator = parser.parse(json).flatMap(_.asJson.as[Boundaries])
    .getOrElse(Boundaries(Nil))
    .boundaries
    .iterator

  override def hasNext: Boolean = iterator.hasNext
  override def next(): Option[OsmDenormalizedObject] = geoJsonToDenormalized(boundaryToFeature(iterator.next()))
}

object OsmGeoJSONBoundaryParser {
  def apply(fileName: String)(implicit codec:Codec): OsmGeoJSONBoundaryParser = OsmGeoJSONBoundaryParser(Source.fromFile(fileName)(codec))
//  implicitly[DecodeJson[List[List[List[Point]]]]]

  def getGeometryFromType(typ:String, c:HCursor) = {
    typ match {
      case "MultiPolygon" ⇒ c.get[List[List[List[(Double, Double)]]]]("coordinates").map[MultiPolygon](MultiPolygon).map[GeometryCollection](mp ⇒ GeometryCollection(List(mp)))
      case "LineString" ⇒ c.get[List[(Double, Double)]]("coordinates").map[LineString](LineString).map[GeometryCollection](ls ⇒ GeometryCollection(List(ls)))
    }
  }


  implicit lazy val  BoundaryDecodeJson = new Decoder[Boundary] {
      override def apply(c: HCursor): Result[Boundary] = for {
      id ← c.get[Int]("id")
      name ← c.get[String]("name")
      localname ← c.get[String]("localname")
      srid ← c.get[String]("SRID")
      adminLevel ← c.get[Int]("admin_level")
      tags ← c.get[Option[Map[String, String]]]("tags")
      typ ← c.get[String]("type")
      geometry ← getGeometryFromType(typ, c)
    } yield Boundary(id, name, localname, srid, adminLevel, tags.getOrElse(Map()), geometry)
  }

  def parseBoundary(s: String) = parser.parse(s).flatMap(_.as[Boundary])

  implicit lazy val BoundariesDecodeJson: Decoder[Boundaries] = Decoder.forProduct1("boundaries")(Boundaries.apply)

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

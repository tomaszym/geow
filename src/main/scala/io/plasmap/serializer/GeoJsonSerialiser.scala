package io.plasmap.serializer

import io.circe.Decoder.Result
import io.circe.Json.JObject
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.semiauto._
import io.plasmap.model.geometry._

object GeoJsonSerialiser {

  implicit lazy val  pointDecode: Decoder[Point] = new Decoder[Point] {
    override def apply(c: HCursor): Result[Point] = c.get[(Double,Double)]("coordinates").map(lonLat => LonLatPoint(lonLat._1, lonLat._2))
  }

  def coordinatesFromPoint(p:Point) = p match {
    case p:LonLatPoint ⇒ (p.lon, p.lat)
    case p:HashPoint ⇒ (p.lon, p.lat)
  }

  implicit lazy val  pointEncode: ObjectEncoder[Point] = new ObjectEncoder[Point] {
    override def encodeObject(a: Point): JsonObject = JsonObject.fromMap(Map(
      "type" -> Json.fromString("Point"),
      "coordinates" -> Json.arr(Json.fromDoubleOrNull(a.lon), Json.fromDoubleOrNull(a.lat))
    ))
}

  implicit lazy val  multiPointDecode = deriveDecoder[MultiPoint]

  implicit lazy val  multiPointEncode = deriveEncoder[MultiPoint].mapJsonObject(_.+:("type" -> Json.fromString("MultiPoint")))

  implicit lazy val  lineStringDecode = deriveDecoder[LineString]

  implicit lazy val  lineStringEncode = deriveEncoder[LineString].mapJsonObject(_.+:("type" -> Json.fromString("LineString")))

  implicit lazy val  multiLineStringDecode = deriveDecoder[MultiLineString]

  implicit lazy val  multiLineStringEncode = deriveEncoder[MultiLineString].mapJsonObject(_.+:("type" -> Json.fromString("MultiLineString")))

  implicit lazy val polygonDecode = deriveDecoder[Polygon]

  implicit lazy val polygonEncode = deriveEncoder[Polygon].mapJsonObject(_.+:("type" -> Json.fromString("Polygon")))

  implicit lazy val multiPolygonDecode = deriveDecoder[MultiPolygon]

  implicit lazy val multiPolygonEncode = deriveEncoder[MultiPolygon].mapJsonObject(_.+:("type" -> Json.fromString("MultiPolygon")))

  implicit lazy val geometryEncoder = new ObjectEncoder[Geometry] {

    override def encodeObject(a: Geometry): JsonObject = a match {
      case p:Point               ⇒ p.asJsonObject
      case mp:MultiPoint         ⇒ mp.asJsonObject
      case ls:LineString         ⇒ ls.asJsonObject
      case mls:MultiLineString   ⇒ mls.asJsonObject
      case poly:Polygon          ⇒ poly.asJsonObject
      case mp:MultiPolygon       ⇒ mp.asJsonObject
      case gc:GeometryCollection ⇒ gc.asJsonObject
    }
  }
  implicit lazy val geometryDecoder = new Decoder[Geometry] {
    override def apply(c: HCursor): Result[Geometry] = {
      c.get[String]("type").flatMap {
        case "Point" ⇒ pointDecode.decodeJson(c.value)
        case "MultiPoint" ⇒ multiPointDecode.decodeJson(c.value)
        case "LineString" ⇒ lineStringDecode.decodeJson(c.value)
        case "MultiLineString" ⇒ multiLineStringDecode.decodeJson(c.value)
        case "Polygon" ⇒ polygonDecode.decodeJson(c.value)
        case "MultiPolygon" ⇒ multiPolygonDecode.decodeJson(c.value)
        case "GeometryCollection" ⇒ geometryCollectionDecode.decodeJson(c.value)
      }
    }
  }

  implicit lazy val geometryCollectionDecode: Decoder[GeometryCollection] = deriveDecoder[GeometryCollection]

  implicit lazy val geometryCollectionEncode: ObjectEncoder[GeometryCollection] = deriveEncoder[GeometryCollection].mapJsonObject(_.+:("type" -> Json.fromString("GeometryCollection")))


  implicit lazy val featureEncoder = deriveEncoder[Feature].mapJsonObject(_.+:("type" -> Json.fromString("Feature")))
  implicit lazy val featureDecoder = deriveDecoder[Feature].prepare { c =>
    c.downField("properties").withFocus { _.mapObject { o =>
      JsonObject.fromMap(o.toMap.mapValues { j =>
        implicit def jsontostringjson(s: String): Json = Json.fromString(s)
        j.fold[Json]("null", _.toString, _.toString, s => s, _.toString, _.toString)
//        Json.fromString(j.toString)
      })
    }

    }.up
  }

  implicit lazy val FeatureCollectionDecode: Decoder[FeatureCollection] = deriveDecoder[FeatureCollection]

  implicit lazy val FeatureCollectionEncode: ObjectEncoder[FeatureCollection] = deriveEncoder[FeatureCollection]

  def geometryFromJSON(json:String)  = parse(json).toOption.flatMap(_.as[Geometry].toOption)
  def jsonFromGeometry(geometry:Geometry) = geometry.asJson.noSpaces
  def jsonFromFeature(feature:Feature) = feature.asJson.noSpaces
  def jsonFromFeatureCollection(featureCollection:FeatureCollection) = featureCollection.asJson.noSpaces
  def featureFromJSON(json:String) = parse(json).toOption.flatMap(_.as[Feature].toOption)
  def featureCollectionFromJSONEither(json:String) = parse(json).flatMap(_.as[FeatureCollection])
  def featureCollectionFromJSON(json:String):Option[FeatureCollection] = parse(json).toOption.flatMap(_.as[FeatureCollection].toOption)

}

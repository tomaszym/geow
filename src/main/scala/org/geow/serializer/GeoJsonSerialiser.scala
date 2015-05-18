package org.geow.serializer

import argonaut._, Argonaut._, Shapeless._
import org.geow.model.geometry._

/**
 * Created by mark on 12.05.15.
 */
object GeoJsonSerialiser {

  private implicit def pointDecode: DecodeJson[Point] =
    DecodeJson( c ⇒ for { lonLat ← (c --\ "coordinates").as[(Double, Double)]} yield LonLatPoint(lonLat._1, lonLat._2))

  def coordinatesFromPoint(p:Point) = p match {
    case p:LonLatPoint ⇒ (p.lon, p.lat)
    case p:HashPoint ⇒ (p.lon, p.lat)
  }

  implicit def pointEncode: EncodeJson[Point] =
    EncodeJson((p:Point) ⇒ ("coordinates" := coordinatesFromPoint(p)) ->: ("type" := "Point") ->: jEmptyObject)

  implicit def multiPointDecode: DecodeJson[MultiPoint] =
    jdecode1L(MultiPoint.apply)("coordinates")

  implicit def multiPointEncode: EncodeJson[MultiPoint] =
    EncodeJson((mp:MultiPoint) ⇒ ("coordinates" := mp.coordinates) ->: ("type" := "MultiPoint") ->: jEmptyObject)

  implicit def lineStringDecode: DecodeJson[LineString] =
    jdecode1L(LineString.apply)("coordinates")

  implicit def lineStringEncode: EncodeJson[LineString] =
    EncodeJson((ls:LineString) ⇒ ("coordinates" := ls.coordinates) ->: ("type" := "LineString") ->: jEmptyObject)

  implicit def multiLineStringDecode: DecodeJson[MultiLineString] =
    jdecode1L(MultiLineString.apply)("coordinates")

  implicit def multiLineStringEncode: EncodeJson[MultiLineString] =
    EncodeJson((mls:MultiLineString) ⇒ ("coordinates" := mls.coordinates) ->: ("type" := "MultiLineString") ->: jEmptyObject)

  implicit def polygonDecode: DecodeJson[Polygon] =
    jdecode1L(Polygon.apply)("coordinates")

  implicit def polygonEncode: EncodeJson[Polygon] =
    EncodeJson((p:Polygon) ⇒ ("coordinates" := p.coordinates) ->: ("type" := "Polygon") ->: jEmptyObject)

  implicit def multiPolygonDecode: DecodeJson[MultiPolygon] =
    jdecode1L(MultiPolygon.apply)("coordinates")

  implicit def multiPolygonEncode: EncodeJson[MultiPolygon] =
    EncodeJson((p:MultiPolygon) ⇒ ("coordinates" := p.coordinates) ->: ("type" := "MultiPolygon") ->: jEmptyObject)

  implicit def geometryCollectionDecode: DecodeJson[GeometryCollection] =
    jdecode1L(GeometryCollection.apply)("geometries")

  implicit def geometryCollectionEncode: EncodeJson[GeometryCollection] =
    EncodeJson((p:GeometryCollection) ⇒ ("geometries" := p.geometries) ->: ("type" := "GeometryCollection") ->: jEmptyObject)

  implicit def geometryCodec:CodecJson[Geometry] =
    CodecJson({
      case p:Point               ⇒ p.asJson
      case mp:MultiPoint         ⇒ mp.asJson
      case ls:LineString         ⇒ ls.asJson
      case mls:MultiLineString   ⇒ mls.asJson
      case poly:Polygon          ⇒ poly.asJson
      case mp:MultiPolygon       ⇒ mp.asJson
      case gc:GeometryCollection ⇒ gc.asJson
    },
    j ⇒ {
      val typ = (j --\ "type").as[String].getOr("Unknown geometry type")
      typ match {
        case "Point"                 ⇒ pointDecode(j)             .map[Geometry](identity)
        case "MultiPoint"            ⇒ multiPointDecode(j)        .map[Geometry](identity)
        case "LineString"            ⇒ lineStringDecode(j)        .map[Geometry](identity)
        case "MultiLineString"       ⇒ multiLineStringDecode(j)   .map[Geometry](identity)
        case "Polygon"               ⇒ polygonDecode(j)           .map[Geometry](identity)
        case "MultiPolygon"          ⇒ multiPolygonDecode(j)      .map[Geometry](identity)
        case "GeometryCollection"    ⇒ geometryCollectionDecode(j).map[Geometry](identity)
      }
    }
    )

  implicit def featureEncode:EncodeJson[Feature] =
    EncodeJson((f:Feature) ⇒ ("properties" := f.properties) ->: ("geometry" := f.geometry) ->: ("type" := "Feature") ->: jEmptyObject)

  implicit def featureDecode:DecodeJson[Feature] =
    DecodeJson( c ⇒ for {
      geometry ← (c --\ "geometry").as[Geometry]
      properties ← (c --\ "properties").as[Option[Map[String, Json]]]
    } yield Feature(geometry, properties.map(_.mapValues{
        case str if str.isString ⇒ str.toString().tail.dropRight(1)
        case other ⇒ other.toString()
      }).getOrElse(Map())))

  implicit def FeatureCollectionDecode:DecodeJson[FeatureCollection] = jdecode1L(FeatureCollection.apply)("features")

  implicit def FeatureCollectionEncode:EncodeJson[FeatureCollection] = jencode1L((fc:FeatureCollection) ⇒ fc.features)("features")

  def geometryFromJSON(json:String)  = json.decodeOption[Geometry]
  def jsonFromGeometry(geometry:Geometry) = geometry.asJson.nospaces
  def jsonFromFeature(feature:Feature) = feature.asJson.nospaces
  def jsonFromFeatureCollection(featureCollection:FeatureCollection) = featureCollection.asJson.nospaces
  def featureFromJSON(json:String) = json.decodeOption[Feature]
  def featureCollectionFromJSONEither(json:String) = json.decodeEither[FeatureCollection]
  def featureCollectionFromJSON(json:String):Option[FeatureCollection] = json.decodeOption[FeatureCollection]

}

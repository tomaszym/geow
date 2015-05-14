package org.geow.parser.impl

import java.io.InputStream

import org.geow.model._
import org.geow.model.geometry._
import org.geow.parser.OsmDenormalizedParser
import play.api.libs.json._
import play.extras.geojson.{Feature, FeatureCollection, Polygon => JsonPolygon, MultiPolygon => JsonMultiPolygon, Geometry => JsonGeometry, GeometryCollection => JsonGeometryCollection, LatLng, LineString => JsonLineString, Point => JsonPoint}

import scala.collection.immutable.Seq
import scala.util.{Random, Failure, Success, Try}

/**
 * Created by janschulte on 06/05/15.
 */
case class OsmGeoJSONParser(json: JsValue) extends OsmDenormalizedParser {

  private val random = new Random()

  private val osmDenormalizedObjects: List[Option[OsmDenormalizedObject]] = init(json)
  private val iterator = osmDenormalizedObjects.iterator

  override def hasNext: Boolean = iterator.hasNext

  override def next(): Option[OsmDenormalizedObject] = iterator.next()


  private def init(json: JsValue): List[Option[OsmDenormalizedObject]] = {
    val featureCollectionResult = parseFeatureCollection(json)

    if (featureCollectionResult == None) {
      val featureResult = parseFeature(json)
      if (featureResult == None) {

        val boundaries: JsValue = json \ "boundaries"
        boundaries match {
          case elements: JsArray => {
            elements.value.map(parseOsmJson).toList
          }
          case _ => List()
        }

      } else {
        featureResult.toList.map(toOsmDenormalizedObject)
      }
    } else {
      featureCollectionResult.get.features.toList.map(toOsmDenormalizedObject)
    }

  }

  private def parseOsmJson(json: JsValue): Option[OsmDenormalizedObject] = {
    val id = (json \ "id").asOpt[Long].getOrElse(random.nextLong())

    val name = (json \ "name").asOpt[String].map(n => "name" -> n)
    val localName = (json \ "localname").asOpt[String].map(ln => "localname" -> ln)
    val adminLevel = (json \ "admin_level").asOpt[String].map(al => "admin_level" -> al)

    val userTags: List[OsmTag] = List(name,localName,adminLevel).flatten.map(tuple => OsmTag.tupleToTag(tuple))

    val tags = (json \ "tags")
    val osmTags = tags match {
      case jsObj: JsObject => jsObj.fields.map(tuple => OsmTag(tuple._1, tuple._2.asOpt[String].getOrElse(""))).toList
      case _ => List()
    }
    Json.fromJson[JsonGeometry[LatLng]](json).map(geometry => toOsmDenormalizedObject(OsmId(id), userTags ::: osmTags , geometry)).asOpt.flatten
  }

  private def parseFeature(json: JsValue): Option[Feature[LatLng]] = {
    Json.fromJson[Feature[LatLng]](json).asOpt
  }

  private def parseFeatureCollection(json: JsValue): Option[FeatureCollection[LatLng]] = {
    Json.fromJson[FeatureCollection[LatLng]](json).asOpt
  }


  private def toOsmDenormalizedObject(feature: Feature[LatLng]): Option[OsmDenormalizedObject with Product with Serializable] = {
    val tags: List[OsmTag] = extractTags(feature)

    val geometry: JsonGeometry[LatLng] = feature.geometry
    toOsmDenormalizedObject(OsmId(random.nextLong()), tags, geometry)
  }

  private def toOsmDenormalizedObject(id: OsmId, tags: List[OsmTag], geometry: JsonGeometry[LatLng]): Option[OsmDenormalizedObject with Product with Serializable] = {
    val geometryOpt = extractGeometry(geometry)

    geometryOpt match {
      case Some(point: Point) =>
        val denormalizedNode: OsmDenormalizedNode = OsmDenormalizedNode(id = id, tags = tags, geometry = point)
        Some(denormalizedNode)

      case Some(linestring: Linestring) =>
        val denormalizedWay = OsmDenormalizedWay(id = id, tags = tags, geometry = linestring)
        Some(denormalizedWay)

      case Some(collection: GeometryCollection) =>
        val denormalizedRelation = OsmDenormalizedRelation(id = id, tags = tags, geometry = collection)
        Some(denormalizedRelation)
      case other =>
        print(other)
        None
    }
  }

  private def extractGeometry(geometry: JsonGeometry[LatLng]): Option[Geometry] = geometry match {
    case jsonPoint: JsonPoint[LatLng] =>
      val coordinates = jsonPoint.coordinates
      Some(extractPoint(coordinates))

    case jsonLineString: JsonLineString[LatLng] =>
      val coordinates = jsonLineString.coordinates.map(extractPoint).toList
      val linestring = Linestring(coordinates)
      Some(linestring)

    case jsonGeometries: JsonGeometryCollection[LatLng] =>
      val membersTry: Seq[Option[GeometryMember]] = jsonGeometries.geometries.map(jsonGeometry => extractGeometry(jsonGeometry).map(toGeometryMember))
      val members: List[GeometryMember] = membersTry.flatten.toList
      val collection: GeometryCollection = GeometryCollection(members)
      Some(collection)
    case jsonPolygon: JsonPolygon[LatLng] =>
      val lines = jsonPolygon.coordinates.map((coords: Seq[LatLng]) => {
        val points = coords.map(extractPoint).toList
        Linestring(points)
      })
      val members = lines.map(line => GeometryMember(OsmTypeWay, OsmId(0), OsmRoleEmpty, line)).toList
      val collection = GeometryCollection(members)
      Some(collection)
    case jsonMultiPolygon: JsonMultiPolygon[LatLng] =>
      val lines = jsonMultiPolygon.coordinates.flatten.map((coords: Seq[LatLng]) => {
        val points = coords.map(extractPoint).toList
        Linestring(points)
      })
      val members = lines.map(line => GeometryMember(OsmTypeWay, OsmId(0), OsmRoleEmpty, line)).toList
      val collection = GeometryCollection(members)
      Some(collection)
    case _ =>
      None
  }


  private def toGeometryMember(geometry: Geometry): GeometryMember = {
    val osmType: OsmType = geometry match {
      case point: Point => OsmTypeNode
      case linestring: Linestring => OsmTypeWay
      case collection: GeometryCollection => OsmTypeRelation
    }

    GeometryMember(osmType, OsmId(0), OsmRoleEmpty, geometry)
  }

  private def extractPoint(coordinates: LatLng): Point = {
    val lon = coordinates.lng
    val lat = coordinates.lat
    Point(lon, lat)
  }

  private def extractTags(feature: Feature[LatLng]): List[OsmTag] = {
    val properties: Option[JsObject] = feature.properties
    val tags: List[OsmTag] = properties match {
      case Some(JsObject(fields)) => fields.collect({ case (k: String, v: JsString) => OsmTag(k, v.value) }).toList
      case None => List()
    }
    tags
  }
}

object OsmGeoJSONParser {

  def apply(source: String): OsmGeoJSONParser = OsmGeoJSONParser(Json.parse(source))

  def apply(inputStream: InputStream): OsmGeoJSONParser = OsmGeoJSONParser(Json.parse(inputStream))
}
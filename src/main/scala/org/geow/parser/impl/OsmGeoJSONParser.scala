package org.geow.parser.impl

import java.io.InputStream

import org.geow.model._
import org.geow.model.geometry._
import org.geow.parser.OsmDenormalizedParser
import play.api.libs.json.{JsString, JsObject, JsValue, Json}
import play.extras.geojson.{Feature, FeatureCollection, Polygon => JsonPolygon, Geometry => JsonGeometry, GeometryCollection => JsonGeometryCollection, LatLng, LineString => JsonLineString, Point => JsonPoint}

import scala.collection.immutable.Seq
import scala.util.{Failure, Success, Try}

/**
 * Created by janschulte on 06/05/15.
 */
case class OsmGeoJSONParser(json: JsValue) extends OsmDenormalizedParser {

  private val featureCollection: FeatureCollection[LatLng] = Json.fromJson[FeatureCollection[LatLng]](json).get
  private val features = featureCollection.features.toList
  private val iterator = features.iterator

  override def hasNext: Boolean = iterator.hasNext

  override def next(): Option[OsmDenormalizedObject] = {
    val feature: Feature[LatLng] = iterator.next()

    val tags: List[OsmTag] = extractTags(feature)

    val geometryOpt = extractGeometry(feature.geometry)

    geometryOpt match {
      case Some(point: Point) =>
        val denormalizedNode: OsmDenormalizedNode = OsmDenormalizedNode(id = OsmId(0), tags = tags, geometry = point)
        Some(denormalizedNode)

      case Some(LineString: LineString) =>
        val denormalizedWay = OsmDenormalizedWay(id = OsmId(0), tags = tags, geometry = LineString)
        Some(denormalizedWay)

      case Some(collection: GeometryCollection) =>
        val denormalizedRelation = OsmDenormalizedRelation(id = OsmId(0), tags = tags, geometry = collection)
        Some(denormalizedRelation)
      case other =>
        print(other)
        None
    }

  }

  def extractGeometry(geometry: JsonGeometry[LatLng]): Option[Geometry] = geometry match {
    case jsonPoint: JsonPoint[LatLng] =>
      val coordinates = jsonPoint.coordinates
      Some(extractPoint(coordinates))

    case jsonLineString: JsonLineString[LatLng] =>
      val coordinates = jsonLineString.coordinates.map(extractPoint).toList
      val LineString = LineString(coordinates)
      Some(LineString)

    case jsonGeometries: JsonGeometryCollection[LatLng] =>
      val membersTry: Seq[Option[GeometryMember]] = jsonGeometries.geometries.map(jsonGeometry => extractGeometry(jsonGeometry).map(toGeometryMember))
      val members: List[GeometryMember] = membersTry.flatten.toList
      val collection: GeometryCollection = GeometryCollection(members)
      Some(collection)
    case jsonPolygon: JsonPolygon[LatLng] =>
      val lines = jsonPolygon.coordinates.map((coords: Seq[LatLng]) => {
        val points = coords.map(extractPoint).toList
        LineString(points)
      })
      val members = lines.map(line => GeometryMember(OsmTypeWay,OsmId(0),OsmRoleEmpty,line)).toList
      val collection = GeometryCollection(members)
      Some(collection)
    case _ =>
      None
  }


  def toGeometryMember(geometry: Geometry): GeometryMember = {
    val osmType: OsmType = geometry match {
      case point: Point => OsmTypeNode
      case LineString: LineString => OsmTypeWay
      case collection: GeometryCollection => OsmTypeRelation
    }

    GeometryMember(osmType, OsmId(0), OsmRoleEmpty, geometry)
  }

  def extractPoint(coordinates: LatLng): Point = {
    val lon = coordinates.lng
    val lat = coordinates.lat
    Point(lon, lat)
  }

  def extractTags(feature: Feature[LatLng]): List[OsmTag] = {
    val properties: Option[JsObject] = feature.properties
    val tags: List[OsmTag] = properties match {
      case Some(JsObject(fields)) => fields.collect({case (k:String,v:JsString) => OsmTag(k,v.value)}).toList
      case None => List()
    }
    tags
  }
}

object OsmGeoJSONParser {

  def apply(source: String): OsmGeoJSONParser = OsmGeoJSONParser(Json.parse(source))

  def apply(inputStream: InputStream): OsmGeoJSONParser = OsmGeoJSONParser(Json.parse(inputStream))
}
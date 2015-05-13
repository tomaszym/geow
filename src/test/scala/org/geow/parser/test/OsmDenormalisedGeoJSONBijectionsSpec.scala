package org.geow.parser.test

import org.geow.geojson.Feature
import org.geow.geojson
import org.geow.model.geometry.{GeometryMember, GeometryCollection, Linestring, Point}
import org.geow.model._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.geow.serializer.OsmDenormalisedGeoJSONBijections._

/**
 * Created by mark on 13.05.15.
 */
class OsmDenormalisedGeoJSONBijectionsSpec extends Specification with ScalaCheck {
  "A simple OsmDenormalizedNode" should
    {
      val node = OsmDenormalizedNode(OsmId(12L), None, OsmVersion(1223L), Nil, Point(0.9999999823048711, 1.9999999855645))
      val geoJson = Feature(org.geow.geojson.Point((0.9999999823048711, 1.9999999855645)),
            Map("_osm_version_changeset" -> "1",
              "_osm_id" -> "12",
              "_osm_user_id" -> "None",
              "_osm_user_name" -> "None",
              "_osm_version_visible" -> "true",
              "_osm_version_timestamp" -> "1223"))

      "successfully map to a GeoJSON Feature" in {
        denormalizedToGeoJson(node) must beEqualTo(geoJson) }

      "successfully map to a GeoJSON Feature and back" in {
        geoJsonToDenormalized(denormalizedToGeoJson(node)) must beSome }

      "successfully be mapped from a Feature" in {
        denormalizedToGeoJson(geoJsonToDenormalized(geoJson).get) must beEqualTo(geoJson)
      }

    }

  "A simple OsmDenormalizedWay" should
    {
      val way = OsmDenormalizedWay(OsmId(12L), Some(OsmUser("fred", 9)), OsmVersion(1223L), Nil, Linestring(
        List( Point(0.9999999823048711, 1.9999999855645), Point(2.000000006519258, 0.9999999613501132))))
      val geoJson = Feature(org.geow.geojson.LineString(List((0.9999999823048711, 1.9999999855645), (2.000000006519258, 0.9999999613501132))),
        Map("_osm_version_changeset" -> "1",
          "_osm_id" -> "12",
          "_osm_user_id" -> "9",
          "_osm_user_name" -> "fred",
          "_osm_version_visible" -> "true",
          "_osm_version_timestamp" -> "1223"))

      "successfully map to a GeoJSON Feature" in {
        denormalizedToGeoJson(way) must beEqualTo(geoJson) }

      "successfully map to a GeoJSON Feature and back" in {
        geoJsonToDenormalized(denormalizedToGeoJson(way)) must beSome }

      "successfully be mapped from a Feature" in {
        denormalizedToGeoJson(geoJsonToDenormalized(geoJson).get) must beEqualTo(geoJson)
      }

      "A simple OsmDenormalizedRelation" should
        {
          val relation = OsmDenormalizedRelation(OsmId(12L), Some(OsmUser("fred", 9)), OsmVersion(1223L), Nil, GeometryCollection(
            List( GeometryMember(OsmTypeNode, OsmId(12L), OsmRoleInner, Point(0, 0)),
                  GeometryMember(OsmTypeWay, OsmId(13L), OsmRoleOuter, Linestring(List(Point(0,0), Point(1,0)))))))
          val geoJson = Feature(org.geow.geojson.LineString(List((0.9999999823048711, 1.9999999855645), (2.000000006519258, 0.9999999613501132))),
            Map("_osm_version_changeset" -> "1",
              "_osm_id" -> "12",
              "_osm_user_id" -> "9",
              "_osm_user_name" -> "fred",
              "_osm_version_visible" -> "true",
              "_osm_version_timestamp" -> "1223"))

          "successfully map to a GeoJSON Feature" in {
            denormalizedToGeoJson(relation) must beEqualTo(geoJson) }

          "successfully map to a GeoJSON Feature and back" in {
            geoJsonToDenormalized(denormalizedToGeoJson(relation)) must beSome }

          "successfully be mapped from a Feature" in {
            denormalizedToGeoJson(geoJsonToDenormalized(geoJson).get) must beEqualTo(geoJson)
          }

        }
    }

}

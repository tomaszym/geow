package org.geow.denormalizer.test

import org.geow.serializer.OsmDenormalizedSerializer
import org.geow.util.Denormalizer
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.geow.model._
import org.geow.model.geometry._
import org.geow.util.Denormalizer._


/**
 * Created by mark on 18.05.15.
 */
class DenormalizerSpec extends Specification with ScalaCheck {

  "Some Nodes, Ways and Relations" should {
    "be denormalized into the right geojson" in {
      //Helper functions:
      def r(id:Int, refs:List[OsmMember]) = OsmRelation(OsmId(id), None, OsmVersion(timestamp = 1L), Nil, refs)
      def w(id:Int, nds:List[Int]) = OsmWay(OsmId(id), None, OsmVersion(timestamp = 1L), Nil, nds.map(OsmId(_)))
      def n(id:Int, xy:(Double, Double)) = OsmNode(OsmId(id), None, OsmVersion(timestamp = 1L), Nil, Point(xy._1, xy._2))

      val nodes: List[OsmNode] = List[(Double, Double)](
        (0, 5), // 0
        (7, 0), // 1
        (12,3), // 2
        (11,10), // 3
        (4, 11), // 4
        (3, 5), // 5
        (7, 2), // 6
        (10,5), // 7
        (6,10), // 8
        (6,5), // 9
        (7,5), // 10
        (7,6), // 11
        (6,6),  // 12
        (16,15), // 13
        (17,15), // 14
        (17,16), // 15
        (16,16)  // 16
      ).zipWithIndex.map(x ⇒ n(x._2, x._1))

      val ways: List[OsmWay] = List(
        List(0,1,2,3,4,0), // 0
        List(5,6,7,8,5), // 1
        List(9,10,11,12,9), // 2
        List(13,14,15), // 3
        List(13,16,15) // 4
      ).zipWithIndex.map(x ⇒ w(x._2, x._1))

      val relations:List[OsmRelation] = List(
        (0, List(
          OsmMember(OsmTypeWay, OsmId(0), OsmRoleOuter),
          OsmMember(OsmTypeWay, OsmId(1), OsmRoleInner),
          OsmMember(OsmTypeWay, OsmId(2), OsmRoleOuter)
        )),
        (1, List(
          OsmMember(OsmTypeWay, OsmId(3), OsmRoleOuter),
          OsmMember(OsmTypeWay, OsmId(4), OsmRoleOuter)

        ))
      ).map(x ⇒ r(x._1, x._2))

      val nodeGeos = nodes.foldLeft(Map.empty[OsmId, Point])(
        (dict, node) ⇒  dict + (node.id → Denormalizer.denormalizeNode(node).geometry)
      )

      val wayGeos = ways.foldLeft(Map.empty[OsmId, LineString])(
        (dict, way) ⇒  dict + (way.id → Denormalizer.denormalizeWay(way, nodeGeos).geometry)
      )

      val relGeos = relations.foldLeft(Map.empty[OsmId, GeometryCollection]) {
        (dict, rel) ⇒
          dict + (rel.id → Denormalizer.denormalizeRelation(rel, nodeGeos, wayGeos, dict).geometry )
      }
      val denNodes = nodes.map(n ⇒ OsmDenormalizedNode(n.id, n.user, n.version, Nil, nodeGeos(n.id)))
      val denWays  = ways .map(w ⇒ OsmDenormalizedWay (w.id, w.user, w.version, Nil,  wayGeos(w.id)))
      val denRels  = relations.map(r ⇒ OsmDenormalizedRelation (r.id, r.user, r.version, Nil,  relGeos(r.id)))
      import org.geow.serializer.{OsmDenormalisedGeoJSONBijections => Bijections, OsmDenormalizedSerializer}
      val geoNodes = denNodes.map(n ⇒ Bijections.denormalizedToGeoJson(n))
      val geoWays  = denWays.map(w ⇒ Bijections.denormalizedToGeoJson(w))
      val geoRels  = denRels.map(r ⇒ Bijections.denormalizedToGeoJson(r))
    OsmDenormalizedSerializer.toGeoJsonString(denRels) must beEqualTo(
      """{"features":[{"type":"Feature","geometry":{"type":"GeometryCollection","geometries":[{"type":"MultiPolygon","coordinates":[[[[17.0,16.0],[16.0,16.0],[16.0,15.0],[17.0,15.0],[17.0,16.0]]]]}]},"properties":{"_osm_version_changeset":"1","_osm_id":"1","_osm_version_visible":"true","_osm_version_timestamp":"1","_osm_user_id":"None","_osm_user_name":"None"}},{"type":"Feature","geometry":{"type":"GeometryCollection","geometries":[{"type":"MultiPolygon","coordinates":[[[[6.0,5.0],[7.0,5.0],[7.0,6.0],[6.0,6.0],[6.0,5.0]]],[[[0.0,5.0],[7.0,0.0],[12.0,3.0],[11.0,10.0],[4.0,11.0],[0.0,5.0]],[[3.0,5.0],[7.0,2.0],[10.0,5.0],[6.0,10.0],[3.0,5.0]]]]}]},"properties":{"_osm_version_changeset":"1","_osm_id":"0","_osm_version_visible":"true","_osm_version_timestamp":"1","_osm_user_id":"None","_osm_user_name":"None"}}]}""").ignoreCase.ignoreSpace
    }
  }
}

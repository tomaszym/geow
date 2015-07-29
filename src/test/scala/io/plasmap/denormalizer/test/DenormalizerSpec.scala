package io.plasmap.denormalizer.test

import io.plasmap.model.{OsmId, OsmTypeWay, OsmMember}
import io.plasmap.util.Denormalizer
import io.plasmap.serializer.OsmDenormalizedSerializer
import io.plasmap.util.Denormalizer
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import io.plasmap.model._
import io.plasmap.model.geometry._
import io.plasmap.util.Denormalizer._

import scala.util.Random


/**
 * Created by mark on 18.05.15.
 */
class DenormalizerSpec extends Specification with ScalaCheck {

  "The Denormaliser" should {
    //Helper functions:
    def r(id:Int, refs:List[OsmMember]) = OsmRelation(OsmId(id), None, OsmVersion(timestamp = 1L), Nil, refs)
    def w(id:Int, nds:List[Int]) = OsmWay(OsmId(id), None, OsmVersion(timestamp = 1L), Nil, nds.map(OsmId(_)))
    def n(id:Int, xy:(Double, Double)) = OsmNode(OsmId(id), None, OsmVersion(timestamp = 1L), Nil, Point(xy._1, xy._2))

    "put one outer into the first position" in {
      val members = List(OsmRoleInner, OsmRoleOuter, OsmRoleInner)
                      .map(OsmMember.curried(OsmTypeWay)(OsmId(Random.nextInt())))
      val sorted = List(members(1), members(0), members(2))
      Denormalizer.sortRefs(members) must beEqualTo(sorted)
    }

    "put many outers into the correct positions" in {
      val members = List(OsmRoleInner, OsmRoleOuter, OsmRoleInner, OsmRoleOuter)
        .map(OsmMember.curried(OsmTypeWay)(OsmId(Random.nextInt())))
      val sorted = List(members(1), members(0), members(3), members(2))
      Denormalizer.sortRefs(members) must beEqualTo(sorted)
    }

    "sort refs in such a way that they always start with an outer" in {
      val members = List(OsmRoleInner, OsmRoleInner, OsmRoleOuter)
                       .map(OsmMember.curried(OsmTypeWay)(OsmId(Random.nextInt())))
      val sorted = List(members(2), members(0), members(1))
      Denormalizer.sortRefs(members) must beEqualTo(sorted)
    }

    "denormalise a simplified version of relation http://osm.org/relation/1360929" in {
      val nodes:List[OsmNode] = List(
        (51.8450736, 6.1846782), //0
        (51.8450808, 6.1848252), //1
        (51.8449223, 6.1848281), //2
        (51.8449294, 6.1846638), //3

        (51.8460303, 6.1852547), //4
        (51.8454490, 6.1846501), //5
        (51.8446737, 6.1845825), //6
        (51.8446655, 6.1851827), //7
        (51.8444029, 6.1852619), //8
        (51.8444030, 6.1856832), //9
        (51.8457537, 6.1875099), //10
        (51.8464680, 6.1857179), //11

        (51.8454522, 6.1848945), //12
        (51.8454463, 6.1855519), //13
        (51.8447022, 6.1855346), //14
        (51.8447081, 6.1848772), //15

        (51.8448600, 6.1846638), //16
        (51.8448636, 6.1848425), //17
        (51.8447229, 6.1848194), //18
        (51.8447318, 6.1846638), //19

        (51.8457815, 6.1856956), //20
        (51.8457352, 6.1872228), //21
        (51.8452398, 6.1863524), //22
        (51.8452609, 6.1856542) //23

      ).map(_.swap).zipWithIndex.map(x ⇒ n(x._2, x._1))

      val ways: List[OsmWay] = List(
        List(0,1,2,3,0),   // 0
        List(16,17,18,19,16), //1
        List(4,5,6,7,8,9,10,11,4), // 2
        List(12,13,14,15,12),  // 3
        List(20,21,22,23,20) // 4
      ).zipWithIndex.map(x ⇒ w(x._2, x._1))

      val relation = r(1360929, List(
        OsmMember(OsmTypeWay, OsmId(0), OsmRoleInner),
        OsmMember(OsmTypeWay, OsmId(1), OsmRoleInner),
        OsmMember(OsmTypeWay, OsmId(2), OsmRoleOuter),
        OsmMember(OsmTypeWay, OsmId(3), OsmRoleInner),
        OsmMember(OsmTypeWay, OsmId(4), OsmRoleInner)
      ))

      val nodeGeos = nodes.foldLeft(Map.empty[OsmId, Point])(
        (dict, node) ⇒  dict + (node.id → Denormalizer.denormalizeNode(node).geometry)
      )

      val wayGeos = ways.foldLeft(Map.empty[OsmId, LineString])(
        (dict, way) ⇒  dict + (way.id → Denormalizer.denormalizeWay(way, nodeGeos).geometry)
      )

      val relGeos:Map[OsmId, GeometryCollection] = Map(relation.id → Denormalizer.denormalizeRelation(relation, nodeGeos, wayGeos, Map.empty).geometry )
      val denNodes = nodes.map(n ⇒ OsmDenormalizedNode(n.id, n.user, n.version, Nil, nodeGeos(n.id)))
      val denWays  = ways .map(w ⇒ OsmDenormalizedWay (w.id, w.user, w.version, Nil,  wayGeos(w.id)))
      val denRel  = OsmDenormalizedRelation (relation.id, relation.user, relation.version, Nil,  relGeos(relation.id))
      import io.plasmap.serializer.OsmDenormalizedSerializer
      import io.plasmap.serializer.{OsmDenormalisedGeoJSONBijections ⇒ Bijections}
      val geoNodes = denNodes.map(n ⇒ Bijections.denormalizedToGeoJson(n))
      val geoWays  = denWays.map(w ⇒ Bijections.denormalizedToGeoJson(w))
      val geoRels  = Bijections.denormalizedToGeoJson(denRel)
      OsmDenormalizedSerializer.toGeoJsonString(denRel) must beEqualTo("""{"type":"Feature","geometry":{"type":"GeometryCollection","geometries":[{"type":"MultiPolygon","coordinates":[[[[6.1852547,51.8460303],[6.1846501,51.845449],[6.1845825,51.8446737],[6.1851827,51.8446655],[6.1852619,51.8444029],[6.1856832,51.844403],[6.1875099,51.8457537],[6.1857179,51.846468],[6.1852547,51.8460303]],[[6.1846638,51.84486],[6.1848425,51.8448636],[6.1848194,51.8447229],[6.1846638,51.8447318],[6.1846638,51.84486]],[[6.1846782,51.8450736],[6.1848252,51.8450808],[6.1848281,51.8449223],[6.1846638,51.8449294],[6.1846782,51.8450736]],[[6.1856956,51.8457815],[6.1872228,51.8457352],[6.1863524,51.8452398],[6.1856542,51.8452609],[6.1856956,51.8457815]],[[6.1848945,51.8454522],[6.1855519,51.8454463],[6.1855346,51.8447022],[6.1848772,51.8447081],[6.1848945,51.8454522]]]]}]},"properties":{"_osm_version_changeset":"1","_osm_id":"1360929","_osm_version_visible":"true","_osm_version_timestamp":"1","_osm_user_id":"None","_osm_user_name":"None"}}""")
    }

    "denormalise a simplified version of relation http://osm.org/relation/1373904 correctly" in {
      val nodes:List[OsmNode] = List(
        (51.1709826, 6.7239214), //0
        (51.1709197, 6.7239349), //1
        (51.1709293, 6.7240489), //2
        (51.1709922, 6.7240353), //3

        (51.1708848, 6.7239052), //4
        (51.1708764, 6.7238241), //5
        (51.1708029, 6.7238433), //6
        (51.1708112, 6.7239245), //7

        (51.1707285, 6.7245784), //8
        (51.1706678, 6.7233803), //9
        (51.1710139, 6.7234070),  //10
        (51.1711302, 6.7244427)  //11

      ).map(_.swap).zipWithIndex.map(x ⇒ n(x._2, x._1))

      val ways: List[OsmWay] = List(
        List(0,1,2,3,0),   // 0
        List(4,5,6,7,4),   // 1
        List(8,9,10,11,8)  // 2
      ).zipWithIndex.map(x ⇒ w(x._2, x._1))

      val relation = r(1373904, List(
        OsmMember(OsmTypeWay, OsmId(0), OsmRoleInner),
        OsmMember(OsmTypeWay, OsmId(1), OsmRoleInner),
        OsmMember(OsmTypeWay, OsmId(2), OsmRoleOuter)
      ))

      val nodeGeos = nodes.foldLeft(Map.empty[OsmId, Point])(
        (dict, node) ⇒  dict + (node.id → Denormalizer.denormalizeNode(node).geometry)
      )

      val wayGeos = ways.foldLeft(Map.empty[OsmId, LineString])(
        (dict, way) ⇒  dict + (way.id → Denormalizer.denormalizeWay(way, nodeGeos).geometry)
      )

      val relGeos:Map[OsmId, GeometryCollection] = Map(relation.id → Denormalizer.denormalizeRelation(relation, nodeGeos, wayGeos, Map.empty).geometry )
      val denNodes = nodes.map(n ⇒ OsmDenormalizedNode(n.id, n.user, n.version, Nil, nodeGeos(n.id)))
      val denWays  = ways .map(w ⇒ OsmDenormalizedWay (w.id, w.user, w.version, Nil,  wayGeos(w.id)))
      val denRel  = OsmDenormalizedRelation (relation.id, relation.user, relation.version, Nil,  relGeos(relation.id))
      import io.plasmap.serializer.OsmDenormalizedSerializer
      import io.plasmap.serializer.{OsmDenormalisedGeoJSONBijections ⇒ Bijections}
      val geoNodes = denNodes.map(n ⇒ Bijections.denormalizedToGeoJson(n))
      val geoWays  = denWays.map(w ⇒ Bijections.denormalizedToGeoJson(w))
      val geoRels  = Bijections.denormalizedToGeoJson(denRel)
      OsmDenormalizedSerializer.toGeoJsonString(denRel).trim() must beEqualTo("""{"type":"Feature","geometry":{"type":"GeometryCollection","geometries":[{"type":"MultiPolygon","coordinates":[[[[6.7245784,51.1707285],[6.7233803,51.1706678],[6.723407,51.1710139],[6.7244427,51.1711302],[6.7245784,51.1707285]],[[6.7239052,51.1708848],[6.7238241,51.1708764],[6.7238433,51.1708029],[6.7239245,51.1708112],[6.7239052,51.1708848]],[[6.7239214,51.1709826],[6.7239349,51.1709197],[6.7240489,51.1709293],[6.7240353,51.1709922],[6.7239214,51.1709826]]]]}]},"properties":{"_osm_version_changeset":"1","_osm_id":"1373904","_osm_version_visible":"true","_osm_version_timestamp":"1","_osm_user_id":"None","_osm_user_name":"None"}}""")
    }

    "denormalise some Nodes, Ways and Relations into their respective geojson" in {

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
      import io.plasmap.serializer.OsmDenormalizedSerializer
      import io.plasmap.serializer.{OsmDenormalisedGeoJSONBijections ⇒ Bijections}
      val geoNodes = denNodes.map(n ⇒ Bijections.denormalizedToGeoJson(n))
      val geoWays  = denWays.map(w ⇒ Bijections.denormalizedToGeoJson(w))
      val geoRels  = denRels.map(r ⇒ Bijections.denormalizedToGeoJson(r))
    OsmDenormalizedSerializer.toGeoJsonString(denRels) must beEqualTo(
      """{"features":[{"type":"Feature","geometry":{"type":"GeometryCollection","geometries":[{"type":"MultiPolygon","coordinates":[[[[17.0,16.0],[16.0,16.0],[16.0,15.0],[17.0,15.0],[17.0,16.0]]]]}]},"properties":{"_osm_version_changeset":"1","_osm_id":"1","_osm_version_visible":"true","_osm_version_timestamp":"1","_osm_user_id":"None","_osm_user_name":"None"}},{"type":"Feature","geometry":{"type":"GeometryCollection","geometries":[{"type":"MultiPolygon","coordinates":[[[[6.0,5.0],[7.0,5.0],[7.0,6.0],[6.0,6.0],[6.0,5.0]]],[[[0.0,5.0],[7.0,0.0],[12.0,3.0],[11.0,10.0],[4.0,11.0],[0.0,5.0]],[[3.0,5.0],[7.0,2.0],[10.0,5.0],[6.0,10.0],[3.0,5.0]]]]}]},"properties":{"_osm_version_changeset":"1","_osm_id":"0","_osm_version_visible":"true","_osm_version_timestamp":"1","_osm_user_id":"None","_osm_user_name":"None"}}]}""").ignoreCase.ignoreSpace
    }
  }

}

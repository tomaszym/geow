package org.geow.util.test

import org.geow.model._
import org.geow.model.geometry._
import org.geow.parser.impl.OsmXmlParser._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class GeoJSONSpec extends Specification with ScalaCheck {

  val id = OsmId(123456789L)
  val user = Some(OsmUser("John_Miller", 123456L))
  val version = OsmVersion(convertXmlDateToLong("2014-04-16T19:23:01Z"), 1, 12345, true)

  val tags = List(
    OsmTag("addr:city", "Düsseldorf"),
    OsmTag("addr:country", "DE"),
    OsmTag("addr:housenumber", "1"),
    OsmTag("addr:postcode", "40200"),
    OsmTag("addr:street", "Heinrich-Heine Allee"))

  val nodePoint = Point(6.7797524, 51.2378913)

  val node = OsmDenormalizedNode(id, user, version, tags, nodePoint)

  val nodeGeoJSON =
    """{"type":"Feature","geometry":{"type":"Point","coordinates":[6.7797524,51.2378913]}, "properties":{"addr:street":"Heinrich-HeineAllee","addr:country":"DE","addr:postcode":"40200","addr:housenumber":"1","addr:city":"Düsseldorf"}}"""


  val lineString = List(
    (6.7797524, 51.2378913),
    (6.7797525, 51.2378914),
    (6.7797526, 51.2378915),
    (6.7797527, 51.2378916),
    (6.7797528, 51.2378917),
    (6.7797529, 51.2378918)
  )

  val way = OsmDenormalizedWay(id, user, version, tags, LineString(lineString))

  val wayGeoJSON =
    """{"type":"Feature","geometry":{"type":"LineString","coordinates":[[6.7797524,51.2378913],[6.7797525,51.2378914],[6.7797526,51.2378915],[6.7797527,51.2378916],[6.7797528,51.2378917],[6.7797529,51.2378918]]},"properties":{"addr:street":"Heinrich-HeineAllee","addr:country":"DE","addr:postcode":"40200","addr:housenumber":"1","addr:city":"Düsseldorf"}}"""

  val relationId = OsmId(91062L)
  val relationUser = Some(OsmUser("Gehrke", 14002L))
  val relationVersion = OsmVersion(convertXmlDateToLong("2013-11-08T12:20:08Z"), 11, 18781052, true)

  val relationTags = List(
    OsmTag("admin_level", "10"),
    OsmTag("boundary", "administrative"),
    OsmTag("name", "Golzheim"),
    OsmTag("type", "boundary"))

  val relationMembers = List(
    OsmMember(OsmTypeWay, OsmId(245181859L), OsmRoleOuter),
    OsmMember(OsmTypeWay, OsmId(245181864L), OsmRoleOuter),
    OsmMember(OsmTypeWay, OsmId(32011174L), OsmRoleOuter),
    OsmMember(OsmTypeWay, OsmId(32011181L), OsmRoleOuter),
    OsmMember(OsmTypeWay, OsmId(32011176L), OsmRoleOuter),
    OsmMember(OsmTypeWay, OsmId(31916345L), OsmRoleOuter),
    OsmMember(OsmTypeWay, OsmId(32011190L), OsmRoleOuter),
    OsmMember(OsmTypeWay, OsmId(32011189L), OsmRoleOuter),
    OsmMember(OsmTypeWay, OsmId(32011184L), OsmRoleOuter))

  val relation = OsmDenormalizedRelation(relationId, relationUser, relationVersion, relationTags, GeometryCollection(
    List(
      Point(102,0.5),
      LineString(List(
        (102.0, 0.0),
        (103.0, 1.0),
        (104.0, 0.0),
        (105.0, 1.0)
      )),
      LineString(List(
        (100.0, 0.0),
        (101.0, 0.0),
        (101.0, 1.0),
        (100.0, 1.0),
        (100.0, 0.0)
      ))
    )
  ))


  val relationGeoJSON =
    """{"type":"Feature","geometry":{"type":"GeometryCollection","geometries":[{"type":"Point","coordinates":[102.0,0.5]},{"type":"LineString","coordinates":[[102.0,0.0],[103.0,1.0],[104.0,0.0],[105.0,1.0]]},{"type":"LineString","coordinates":[[100.0,0.0],[101.0,0.0],[101.0,1.0],[100.0,1.0],[100.0,0.0]]}]},"properties":{"admin_level":"10","boundary":"administrative","name":"Golzheim","type":"boundary"}}"""

  "The GeoJSON" should {

    "should transform an OsmDenormalizedNode into GeoJSON" in {
      val actualGeoJSON = node.toGeoJson(withTags = true)
      actualGeoJSON must be_==(nodeGeoJSON).ignoreCase.ignoreSpace
    }

    "should transform an OsmDenormalizedWay into GeoJSON" in {
      val actualGeoJSON = way.toGeoJson(withTags = true)
      actualGeoJSON must be_==(wayGeoJSON).ignoreCase.ignoreSpace
    }

    "should transform an OsmDenormalizedRelation into GeoJSON" in {
      val actualGeoJSON = relation.toGeoJson(withTags = true)
      actualGeoJSON must be_==(relationGeoJSON).ignoreCase.ignoreSpace
    }
  }

}
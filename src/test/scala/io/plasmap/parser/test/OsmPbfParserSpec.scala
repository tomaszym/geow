package io.plasmap.parser.test

/**
 * Created by jm on 15/07/15.
 */

import java.nio.charset.{StandardCharsets}
import java.nio.file.{StandardOpenOption, Files}

import io.plasmap.model.{OsmRoleOuter, OsmTypeWay}
import org.specs2.mutable.Specification

import java.io.File
import org.openstreetmap.osmosis.core.Osmosis
import org.junit.rules.TemporaryFolder

import org.specs2.runner._
import org.junit.runner._

import org.specs2.ScalaCheck

import io.plasmap.parser.impl.OsmPbfParser
import io.plasmap.parser.impl.OsmXmlParser._
import io.plasmap.model._
import io.plasmap.model.geometry.Point

@RunWith(classOf[JUnitRunner])
class OsmPbfParserSpec extends Specification with ScalaCheck {

  val xml =
    <osm version="0.6" generator="CGImap 0.3.3 (4443 thorn-02.openstreetmap.org)" copyright="OpenStreetMap and contributors" attribution="http://www.openstreetmap.org/copyright" license="http://opendatacommons.org/licenses/odbl/1-0/">
      <node id="2465725143" visible="true" version="2" changeset="21736329" timestamp="2014-04-16T19:23:01Z" user="black_bike" uid="18130" lat="51.2378913" lon="6.7797524">
        <tag k="addr:city" v="Düsseldorf"/>
        <tag k="addr:country" v="DE"/>
        <tag k="addr:housenumber" v="53"/>
        <tag k="addr:postcode" v="40477"/>
        <tag k="addr:street" v="Nordstraße"/>
        <tag k="amenity" v="restaurant"/>
        <tag k="cuisine" v="regional"/>
        <tag k="name" v="Himmel und Ähd"/>
        <tag k="phone" v="+49 211 4981361"/>
        <tag k="website" v="http://www.himmel-aehd.de"/>
      </node>
      <way id="143653722" visible="true" version="5" changeset="19982704" timestamp="2014-01-14T00:57:49Z" user="teufli" uid="247886">
        <nd ref="203790573"/>
        <nd ref="717638289"/>
        <nd ref="73664701"/>
        <nd ref="827539061"/>
        <nd ref="717638282"/>
        <nd ref="827538924"/>
        <nd ref="73664703"/>
        <nd ref="717638284"/>
        <nd ref="717638278"/>
        <tag k="highway" v="trunk"/>
        <tag k="lanes" v="2"/>
        <tag k="layer" v="-1"/>
        <tag k="lit" v="yes"/>
        <tag k="lit_by_gaslight" v="no"/>
        <tag k="maxspeed" v="80"/>
        <tag k="maxspeed:conditional" v="60 @ (22:00-06:00)"/>
        <tag k="motorroad" v="yes"/>
        <tag k="name" v="Rheinalleetunnel"/>
        <tag k="oneway" v="yes"/>
      </way>
      <relation id="91062" visible="true" version="11" changeset="18781052" timestamp="2013-11-08T12:20:08Z" user="Gehrke" uid="14002">
        <member type="way" ref="245181859" role="outer"/>
        <member type="way" ref="245181864" role="outer"/>
        <member type="way" ref="32011174" role="outer"/>
        <member type="way" ref="32011181" role="outer"/>
        <member type="way" ref="32011176" role="outer"/>
        <member type="way" ref="31916345" role="outer"/>
        <member type="way" ref="32011190" role="outer"/>
        <member type="way" ref="32011189" role="outer"/>
        <member type="way" ref="32011184" role="outer"/>
        <tag k="admin_level" v="10"/>
        <tag k="boundary" v="administrative"/>
        <tag k="name" v="Golzheim"/>
        <tag k="type" v="boundary"/>
        <tag k="wikipedia" v="de:Golzheim_(DÃ¼sseldorf)"/>
      </relation>
    </osm>

  val nodeId = OsmId(2465725143L)
  val nodeUser = Some(OsmUser("black_bike", 18130L))
  val nodeVersion = OsmVersion(convertXmlDateToLong("2014-04-16T19:23:01Z"), 2, 21736329, true)

  val nodeTags = List(
    OsmTag("addr:city", "Düsseldorf"),
    OsmTag("addr:country", "DE"),
    OsmTag("addr:housenumber", "53"),
    OsmTag("addr:postcode", "40477"),
    OsmTag("addr:street", "Nordstraße"),
    OsmTag("amenity", "restaurant"),
    OsmTag("cuisine", "regional"),
    OsmTag("name", "Himmel und Ähd"),
    OsmTag("phone", "+49 211 4981361"),
    OsmTag("website", "http://www.himmel-aehd.de"))

  val nodePoint = Point(6.7797524, 51.2378913)

  val node = OsmNode(nodeId,nodeUser,nodeVersion, nodeTags, nodePoint)

  val wayId = OsmId(143653722L)
  val wayUser = Some(OsmUser("teufli", 247886L))
  val wayVersion = OsmVersion(convertXmlDateToLong("2014-01-14T00:57:49Z"), 5, 19982704, true)

  val wayTags = List(
    OsmTag("highway", "trunk"),
    OsmTag("lanes", "2"),
    OsmTag("layer", "-1"),
    OsmTag("lit", "yes"),
    OsmTag("lit_by_gaslight", "no"),
    OsmTag("maxspeed", "80"),
    OsmTag("maxspeed:conditional", "60 @ (22:00-06:00)"),
    OsmTag("motorroad", "yes"),
    OsmTag("name", "Rheinalleetunnel"),
    OsmTag("oneway", "yes"))

  val wayNds = List(
    OsmId(203790573L),
    OsmId(717638289L),
    OsmId(73664701L),
    OsmId(827539061L),
    OsmId(717638282L),
    OsmId(827538924L),
    OsmId(73664703L),
    OsmId(717638284L),
    OsmId(717638278L))

  val way = OsmWay(wayId,wayUser,wayVersion, wayTags, wayNds)

  val relationId = OsmId(91062L)
  val relationUser = Some(OsmUser("Gehrke", 14002L))
  val relationVersion = OsmVersion(convertXmlDateToLong("2013-11-08T12:20:08Z"), 11, 18781052, true)

  val relationTags = List(
    OsmTag("admin_level", "10"),
    OsmTag("boundary", "administrative"),
    OsmTag("name", "Golzheim"),
    OsmTag("type", "boundary"),
    OsmTag("wikipedia", "de:Golzheim_(DÃ¼sseldorf)"))

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

  val relation = OsmRelation(relationId, relationUser, relationVersion , relationTags, relationMembers)


  def makePbfFile(folder : TemporaryFolder) : File = {
    val pbfFile = folder.newFile("test.osm.pbf")
    val xmlFile = folder.newFile("test.osm")
    Files.write(xmlFile.toPath, xml.toString.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE)
    Osmosis.run(Array(
      "-q",
      "--read-xml-0.6",
      xmlFile.getPath(),
      "--write-pbf-0.6",
      pbfFile.getPath()
    ))

    pbfFile
  }

  "The OsmPbfParser" should {

    "parse an Osm pbf" in {
      val folder = new TemporaryFolder()
      folder.create()
      val parserPbf = OsmPbfParser(makePbfFile(folder).getAbsolutePath)

      parserPbf.hasNext must be_==(true)
      val n = parserPbf.next
      n.get.id mustEqual node.id
      n.get.user mustEqual node.user
      n.get.version mustEqual node.version
      n.get.tags must containAllOf(node.tags)

      parserPbf.hasNext must be_==(true)
      val w = parserPbf.next
      w.get.id mustEqual way.id
      w.get.user mustEqual way.user
      w.get.version mustEqual way.version
      w.get.tags must containAllOf(way.tags)

      parserPbf.hasNext must be_==(true)
      val r = parserPbf.next
      folder.delete()
      r.get.id mustEqual relation.id
      r.get.user mustEqual relation.user
      r.get.version mustEqual relation.version
      r.get.tags must containAllOf(relation.tags)
    }
  }

}

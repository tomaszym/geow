package io.plasmap.generator

import io.plasmap.model._
import io.plasmap.model.geometry._

import scala.util.Random

/**
 * Utility class to generate osm objects
 *
 * @author Jan Schulte <jan@plasmap.io>
 */
case class OsmObjectGenerator() {

  val random = new Random()

  def generateOsmId = OsmId(random.nextLong)

  def generateUser: Option[OsmUser] = {
    Some(OsmUser(random.nextString(10), random.nextLong))
  }

  def generateVersion: OsmVersion = {
    val num = random.nextInt(10)
    val cs = random.nextInt(100000)
    OsmVersion(versionNumber = num, changeset = cs, visible = true)
  }

  def generateTag: OsmTag = {
    val key = random.nextString(10)
    val value = random.nextString(10)
    OsmTag(key, value)
  }

  def generateTags(n: Int = 10): List[OsmTag] = {
    Seq.fill(n)(generateTag).toList
  }

  def generateNds(n: Int = 20): List[OsmId] = {
    Seq.fill(n)(generateOsmId).toList
  }

  def generateMember: OsmMember = {
    val `type` = generateOsmType
    val ref = generateOsmId
    val role = generateOsmRole
    OsmMember(`type`, ref, role)
  }

  def generateMembers(n: Int = 10): List[OsmMember] = {
    Seq.fill(n)(generateMember).toList
  }

  def generatePoint: Point = {
    val lon = random.nextDouble * 360 - 180
    val lat = random.nextDouble * 180 - 90
    Point(lon, lat)
  }

  def generatePointList(n: Int = 20): List[Point] = {
    Seq.fill(n)(generatePoint).toList
  }

  def generateOsmType: OsmType = {
    oneOf(OsmTypeNode, OsmTypeWay, OsmTypeRelation)
  }

  def oneOf[T](params: T*): T = Random.shuffle(params).head

  def generateOsmRole: OsmRole = {
    oneOf(OsmRoleEmpty, OsmRoleInner, OsmRoleOuter)
  }

  def generateLinestring: LineString = {
    LineString(generatePointList().map(p ⇒ (p.lon, p.lat)))
  }

  def generatePolygon: Polygon = {
    val n = random nextInt 4
    val lineStrings = for(x ← 1 until n) yield generateLinestring
    Polygon(lineStrings.toList.map(_.coordinates))
  }

  def generateMultiPolygon: MultiPolygon = {
    val n = random nextInt 4
    val polygons = for(x ← 1 until n) yield generatePolygon
    MultiPolygon(polygons.toList.map(_.coordinates))
  }

  def generateGeometry:Geometry = {
    val possibilities = List( generateMultiPolygon, generateLinestring, generatePoint )
    val which = random nextInt possibilities.length
    possibilities(which)
  }

  def generateGeometryCollection:GeometryCollection = {
    val pointsN = random.nextInt(4)
    val lineStringsN = random.nextInt(2)
    GeometryCollection(
     List(generateMultiPolygon) ++ List.fill(lineStringsN)(generateLinestring) ++ List.fill(pointsN)(generatePoint)
    )
  }

  def generateNode: OsmNode = {
    val id = generateOsmId
    val user = generateUser
    val version = generateVersion
    val tags = generateTags()
    val point = generatePoint
    OsmNode(id, user, version, tags, point)
  }

  def generateWay: OsmWay = {
    val id = generateOsmId
    val user = generateUser
    val version = generateVersion
    val tags = generateTags()
    val nds = generateNds()
    OsmWay(id, user, version, tags, nds)
  }

  def generateRelation: OsmRelation = {
    val id = generateOsmId
    val user = generateUser
    val version = generateVersion
    val tags = generateTags()
    val members = generateMembers()
    OsmRelation(id, user, version, tags, members)
  }

  def generateDenormalizedNode: OsmDenormalizedNode = {
    val id = generateOsmId
    val user = generateUser
    val version = generateVersion
    val tags = generateTags()
    val point = generatePoint
    OsmDenormalizedNode(id, user, version, tags, point)
  }

  def generateDenormalizedWay: OsmDenormalizedWay = {
    val id = generateOsmId
    val user = generateUser
    val version = generateVersion
    val tags = generateTags()
    val geometryWay = generateLinestring
    OsmDenormalizedWay(id, user, version, tags, geometryWay)
  }

  def generateDenormalizedRelation: OsmDenormalizedRelation = {
    val id = generateOsmId
    val user = generateUser
    val version = generateVersion
    val tags = generateTags()
    val geometryCollection = generateGeometryCollection
    OsmDenormalizedRelation(id, user, version, tags, geometryCollection)
  }

}
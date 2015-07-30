package io.plasmap.util

import io.plasmap.model.geometry.GeometryUtil
import io.plasmap.model.{OsmTypeRelation, OsmTypeWay, OsmTypeNode, OsmId}
import io.plasmap.model._
import io.plasmap.model.geometry._

import scala.annotation.tailrec

/**
 * Utility class to denormalize osm elements.
 * @author Jan Schulte <jan@plasmap.io>
 */
object Denormalizer {

  def denormalizeNode(node: OsmNode): OsmDenormalizedNode = {
    OsmDenormalizedNode(node.id, node.user, node.version, node.tags, node.point)
  }

  def denormalizeWay(way: OsmWay, mappings: Map[OsmId, Point]): OsmDenormalizedWay = {
    val wayPoints = for {
      nd: OsmId <- way.nds
      point <- mappings.get(nd)
      lon = point.lon
      lat = point.lat
    } yield (lon, lat)

    val geometry = LineString(wayPoints)
    OsmDenormalizedWay(way.id,way.user,way.version, way.tags, geometry)
  }

  def sortRefs(members:List[OsmMember]):List[OsmMember] = {
    if(members.headOption.map(_.role == OsmRoleOuter).getOrElse(true)) //First is outer, that's fine.
      members
    else { //First is not an outer
    val (inners, outers) = members.partition(_.role == OsmRoleInner)
      if(outers == Nil) //There are no outers at all
        inners.map(_.copy(role = OsmRoleOuter))
      else
        outers ++ inners //Have the outers first and then the inners

//      def go(members: List[OsmMember], prevWasInner: Boolean, ordered: List[List[OsmMember]]): List[OsmMember] = {
//        if (members isEmpty) ordered.flatMap(_.reverse).reverse
//        else {
//          val isInner = members.head.role == OsmRoleInner
//          if(prevWasInner && ordered.nonEmpty)
//            go(members.tail, isInner, (members.head :: ordered.head) :: ordered.tail)
//          else // was outer or ordered is Empty
//            go(members.tail, isInner, List(members.head) :: ordered)
//        }
//      }
//      go(members, prevWasInner = false, Nil)
    }
  }

  def denormalizeRelation(relation: OsmRelation, nodes:Map[OsmId, Point], ways:Map[OsmId, LineString], rels:Map[OsmId, GeometryCollection]):OsmDenormalizedRelation = {

    @inline def addLinearRing(isInner:Boolean, lineString:List[(Double, Double)], mp:List[List[List[(Double, Double)]]], ps:List[Point], ws:List[List[(Double, Double)]], members:List[OsmMember] ):GeometryCollection =  {
      val newMp: List[List[List[(Double, Double)]]] =
        if (isInner) //subtract this one from the latest
          (mp.head :+ lineString) :: mp.tail
        else //create a new polygon
          (lineString :: Nil) :: mp
      go(isInner, members.tail, newMp, ps, ws)
    }

    @tailrec
    def go(lastWasInner:Boolean, members:List[OsmMember], mp:List[List[List[(Double, Double)]]], ps:List[Point], ws:List[List[(Double, Double)]]):GeometryCollection = {
      if(members == Nil) // Base case: We're done
        GeometryCollection(MultiPolygon(mp) :: ps ++ ws.map(LineString))
      else {
        val member = members.head
        val isInner = member.role == OsmRoleInner
//        def repeat = go(isInner, members.tail) //IntelliJ whines when doing this about it not being tail recursive
        member.typ match {
          case OsmTypeNode ⇒ go(isInner, members.tail, mp, nodes(member.ref) :: ps, ws)

          case OsmTypeWay ⇒
            import GeometryUtil._
            val lineString: List[(Double, Double)] = ways(member.ref).coordinates
            if(isLinearRing(lineString))
              addLinearRing(isInner, lineString, mp, ps, ws, members)
            else {
              val joinedOpt = for {
                head ← ws.headOption
                joined ← attemptJoinLineStrings(lineString, head)
              } yield joined

              if(joinedOpt.exists(isLinearRing))
                addLinearRing(isInner, joinedOpt.get, mp, ps, ws.tail, members)
              else {
                val newWays = joinedOpt.map(_ :: ws.tail).getOrElse(lineString :: ws)
                go(isInner, members.tail, mp, ps, newWays)
              }
            }

          case OsmTypeRelation ⇒ //TODO: Check roles
            val geometries = rels(member.ref).geometries
            val multiPoly = geometries.collectFirst{ case mp:MultiPolygon ⇒ mp}.getOrElse(MultiPolygon(Nil)).coordinates
            val points: List[Point] = geometries.collect{ case p:Point ⇒ p }
            go(isInner, members.tail, multiPoly ++ mp, ps ++ points, ws)
        }
      }
    }
    val sortedRefs = sortRefs(relation.refs)
    OsmDenormalizedRelation(relation.id, relation.user, relation.version, relation.tags,
      go(lastWasInner = false, sortedRefs, Nil, Nil, Nil))
  }
}

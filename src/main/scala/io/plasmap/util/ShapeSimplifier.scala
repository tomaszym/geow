package io.plasmap.util

import io.plasmap.model.{OsmDenormalizedRelation, OsmDenormalizedNode, OsmDenormalizedWay, OsmDenormalizedObject}
import io.plasmap.model.geometry.{GeometryCollection, MultiPolygon, LineString}

import scala.math.BigDecimal

/**
  * Created by mark on 18.11.15.
  */
object ShapeSimplifier {
  def perpendicularDistance(point:(Double, Double), line:((Double,Double), (Double, Double))):Double = {
    @inline val x0 = point._1
    @inline val y0 = point._2
    @inline val x1 = line._1._1
    @inline val y1 = line._1._2
    @inline val x2 = line._2._1
    @inline val y2 = line._2._2

    Math.abs( (y2 - y1)*x0 - (x2-x1)*y0 + x2*y1 - y2*x1 ) / Math.sqrt( Math.pow(y2 - y1, 2) + Math.pow(x2 - x1, 2) )
  }

  def simplify[A <: OsmDenormalizedObject](osmDenormalizedObject:A, ε:Double):A = {
    def node(n:OsmDenormalizedNode):OsmDenormalizedNode = n

    def way(w:OsmDenormalizedWay):OsmDenormalizedWay = {
      val simplified = douglasPeucker(w.geometry.coordinates, ε)
      w.copy(geometry = w.geometry.copy(simplified))
    }

    def relation(r:OsmDenormalizedRelation):OsmDenormalizedRelation = {
      val polys = r.geometry.geometries.flatMap{
        case MultiPolygon(coords) => coords.headOption
        case _                                               => None
      }.headOption
      polys.fold(r)( //Don't do nothing
       ps => {
         val simplified = ps.map(douglasPeucker(_, ε))
         val geo = MultiPolygon(List(simplified))
         r.copy(geometry = r.geometry.copy( geo :: Nil ))
       }
      )
    }

    osmDenormalizedObject.fold( node, way, relation ).asInstanceOf[A]

  }

  def douglasPeucker(geometry:List[(Double, Double)], ε:Double):List[(Double, Double)] = {

    def go(pointList:Seq[(Double, Double)]):Seq[(Double, Double)] = {
      if(pointList.size <= 2) pointList
      else {
        val (dmax, index) = (1 until pointList.size-1).foldLeft((0.0, 0)) {
          case ((_dmax, _index), i) =>
            val d = perpendicularDistance(pointList(i), (pointList.head, pointList.last))
            if( d > _dmax)
              (d, i)
            else
              (_dmax, _index)
        }

        if( dmax > ε ) {
          val (left, right) = pointList.splitAt(index)
          go(left) ++ go(right)
        }
        else
          List(pointList.head, pointList.last)
      }
    }

    val result = if(geometry.head == geometry.last) { //Closed linear ring.
        geometry.head :: go(geometry.tail).toList
    } else go(geometry).toList

    result
  }

}

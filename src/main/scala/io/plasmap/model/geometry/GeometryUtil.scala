package io.plasmap.model.geometry

/**
 * Created by mark on 15.05.15.
 */
object GeometryUtil {
  def attemptJoinLineStrings(lineString1:List[(Double, Double)],
                             lineString2:List[(Double, Double)]):Option[List[(Double, Double)]] = {
    if(lineString1.length < 2 || lineString2.length < 2) None
    else {
      val ls1Start = lineString1.head
      val ls1End = lineString1.last
      val ls2Start = lineString2.head
      val ls2End = lineString2.last

      if     (ls1Start == ls2Start) Some(lineString1.reverse ++ lineString2.tail)
      else if(ls1Start == ls2End)   Some(lineString2 ++ lineString1.tail)
      else if(ls1End == ls2Start)   Some(lineString1 ++ lineString2.tail)
      else if(ls1End == ls2End)     Some(lineString1 ++ lineString2.reverse.tail)
      else                          None
    }
  }
  def attemptJoinLineStrings(ls1:LineString, ls2:LineString):Option[LineString] =
    attemptJoinLineStrings(ls1.coordinates, ls2.coordinates).map(LineString)

  def isLinearRing(ls:List[(Double, Double)]):Boolean = ls.length >= 4 && ls.head == ls.last
  def isLinearRing(ls:LineString):Boolean = isLinearRing(ls.coordinates)
}

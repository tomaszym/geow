package org.geow.model.geometry.test

import org.geow.model.geometry.{LineString, Point}
import org.geow.model.{OsmDenormalizedRelation, OsmDenormalizedWay, OsmDenormalizedNode}
import org.geow.generator.OsmObjectGenerator
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

/**
 * Created by janschulte on 12/02/15.
 */
class PrintingSpec extends Specification with ScalaCheck {

  val generator = OsmObjectGenerator()

  "The GeoHash" should {

    "pretty print an OsmPoint" in {
      val p = Point(34.4344583453, 43.3405834580345)
      val pretty = p.toString
      pretty must be_==("""{"type":"Point","coordinates":[34.4344583453,43.3405834580345]}""")
    }

    "pretty print a list of OsmPoints" in {
      val p1 = Point(34.4344583453, 43.3405834580345)
      val p2 = Point(34.34083453, 43.330345)
      val p3 = Point(35.5458567567, 45.34534580345)
      val w = LineString(List(p1,p2,p3).map(p ⇒ (p.lon, p.lat)))
      val pretty = w.toString
      pretty must be_==("""{"type":"LineString","coordinates":[[34.4344583453,43.3405834580345],[34.34083453,43.330345],[35.5458567567,45.34534580345]]}""")
    }
  }

}
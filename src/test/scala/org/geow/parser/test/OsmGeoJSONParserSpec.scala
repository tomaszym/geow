package org.geow.parser.test

import org.geow.parser.OsmDenormalizedParser
import org.junit.runner._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class OsmGeoJSONParserSpec extends Specification with ScalaCheck {


  val linestrings = OsmDenormalizedParser("src/test/resources/ways.geojson")
  val multiPolygons = OsmDenormalizedParser("src/test/resources/districts.muehlheim.geojson")

  "The OsmDenormalizedParser" should {

    "parse a geojson with linestrings" in {

      val elements = (for(elem <- linestrings) yield elem).toList
      for(elem <- elements) println(elem)

      elements must have size(82)
    }

    "parse a geojson with multi-polygons" in {

      val elements = (for(elem <- multiPolygons) yield elem).toList

      for(elem <- elements) println(elem)

      elements must have size(19)
    }
  }

}
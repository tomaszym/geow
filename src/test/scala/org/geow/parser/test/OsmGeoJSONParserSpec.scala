package org.geow.parser.test

import org.geow.parser.OsmDenormalizedParser
import org.junit.runner._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.runner._

@RunWith(classOf[JUnitRunner])
class OsmGeoJSONParserSpec extends Specification with ScalaCheck {


  val parser = OsmDenormalizedParser("src/test/resources/ways.geojson")

  "The OsmObjectParser" should {

    "parse an Osm xml" in {

      for(elem <- parser) println(elem)
      true must beTrue
    }
  }

}
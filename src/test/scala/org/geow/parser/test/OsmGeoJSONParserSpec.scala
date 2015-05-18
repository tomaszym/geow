package org.geow.parser.test

import argonaut.Argonaut._
import argonaut._
import org.geow.model.geometry.FeatureCollection
import org.geow.parser.impl.OsmGeoJSONParser
import org.geow.serializer.GeoJsonSerialiser
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class OsmGeoJSONParserSpec extends Specification with ScalaCheck {


  val linestrings = OsmGeoJSONParser("src/test/resources/ways.geojson")


  "The OsmGeoJSONParser" should {

    "parse a geojson with linestrings" in {
      val x = OsmGeoJSONParser("src/test/resources/ways.geojson").json
      println(GeoJsonSerialiser.featureCollectionFromJSONEither(x))
      val elements = (for(elem <- linestrings) yield elem).toList
//      for(elem <- elements) println(elem)

      elements must have size 82
    }

  }

}
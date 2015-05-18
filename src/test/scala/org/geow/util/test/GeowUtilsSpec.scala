package org.geow.util.test

import org.geow.generator.OsmObjectGenerator
import org.geow.model.OsmId
import org.geow.model.geometry.Point
import org.geow.util.Denormalizer
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class GeowUtilsSpec extends Specification with ScalaCheck {

  sequential

  val generator = OsmObjectGenerator()
  import generator._

  "The GeowUtils" should {

    "denormalize an \"OsmWay\"" in {

      val expectedWay = generateWay
      val expectedNds = expectedWay.nds
      val expectedId = expectedWay.id
      val expectedUser = expectedWay.user
      val expectedVersion = expectedWay.version
      val expectedTags = expectedWay.tags

      val expectedMappingsList = for (nd <- expectedNds) yield nd -> generatePoint
      val expectedMappings: Map[OsmId, Point] = expectedMappingsList.toMap

      val denormalizedWay = Denormalizer.denormalizeWay(expectedWay, expectedMappings)
      val actualId = denormalizedWay.id
      val actualUser = denormalizedWay.user
      val actualVersion = denormalizedWay.version
      val actualTags = denormalizedWay.tags
      val actualGeometry = denormalizedWay.geometry
      val actualPoints = actualGeometry.coordinates

      for (nd <- expectedNds) {
        val expectedPoint = expectedMappings(nd)
        val expectedTuple = (expectedPoint.lon, expectedPoint.lat)
        actualPoints must contain(expectedTuple)
      }

      actualTags must containTheSameElementsAs(expectedTags)
      actualId must be_==(expectedId)
      actualUser must be_==(expectedUser)
      actualVersion must be_==(expectedVersion)
    }

  }

}
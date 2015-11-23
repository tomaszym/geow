package io.plasmap.util.test

import io.plasmap.model.{OsmDenormalizedRelation, OsmVersion, OsmId, OsmDenormalizedWay}
import io.plasmap.model.geometry.{MultiPolygon, LineString}
import io.plasmap.serializer.{GeoJsonSerialiser, OsmDenormalisedGeoJSONBijections, OsmDenormalizedSerializer}
import io.plasmap.util.ShapeSimplifier
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import scalaz.syntax.id._

import scala.reflect.io.File

/**
  * Created by mark on 18.11.15.
  */
class ShapeSimplifierSpec extends Specification with ScalaCheck {
  "The Simplifier" should {

    "correctly compute distances" in {
      ShapeSimplifier.perpendicularDistance((1, 1), ((0, 0), (2, 0))) must_== 1.0
    }

    "correctly compute distances again" in {
      ShapeSimplifier.perpendicularDistance((0, 2), ((0, 0), (2, 2))) must beCloseTo(Math.sqrt(2), 0.0001)
    }

    def makeWay(coordinates: List[(Double, Double)]) = {
      OsmDenormalizedWay(OsmId(1), tags = Nil, geometry = LineString(coordinates))
    }

    "simplify a simple shape with right epsilon" in {
      val way = makeWay(List((0.0, 0.0), (1.0, 0.0), (1.0, 1.0)))
      val simplified = ShapeSimplifier.simplify(way, 2.0)
      println(simplified)
      simplified must_== way.copy(geometry = LineString(List((0.0, 0.0), (1.0, 1.0))))

      val same = ShapeSimplifier.simplify(way, 0.5)
      same must_== way
    }

    "simplify with many epsilons" in {
      val cologneString = scala.io.Source.fromFile("src/test/resources/cologne.geojson").mkString
      val cologneObject = for {
        feature <- GeoJsonSerialiser.featureFromJSON(cologneString)
        denorma <- OsmDenormalisedGeoJSONBijections.geoJsonToDenormalized(feature)
      } yield denorma.asInstanceOf[OsmDenormalizedRelation]
      val epsilonsAndMaxPoints = List(
        (0.5,           10),
        (0.1,           10),
        (0.05,         100),
        (0.01,         100),
        (0.005,        400),
        (0.001,        400),
        (0.0005,      1400),
        (0.0001,      1400),
        (0.00005,     2400),
        (0.00001,     2400),
        (0.000005,    3100),
        (0.000001,    3100),
        (0.0000005,   4400),
        (0.0000001,   4400),
        (0.00000005,  5000),
        (0.00000001,  5000),
        (0.000000005, 5020),
        (0.000000001, 5020),
        (0.0000000005,5030),
        (0.0000000001,5030)
      )
      for{
        (e,maximumPoints:Int) <- epsilonsAndMaxPoints
        simplified = ShapeSimplifier.simplify(cologneObject.get, e)
        points = simplified.geometry.geometries.head.asInstanceOf[MultiPolygon].coordinates.head.head.size // :)
        _ = println(s"$e\t$points")
      } yield points must be_<=(maximumPoints)

    }
  }
}

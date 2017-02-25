package io.plasmap.serializer.test

import io.plasmap.generator.OsmObjectGenerator
import io.plasmap.model._
import io.plasmap.serializer.OsmDenormalizedSerializer._
import org.junit.runner._
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import org.specs2.runner._
import org.scalacheck.Prop.forAll

@RunWith(classOf[JUnitRunner])
class OsmDenormalizedSerializerTest extends Specification with ScalaCheck {

  sequential
  
  val generator = OsmObjectGenerator()
  
  def denormalizedNodeGenerator = Gen.resultOf[Int,OsmDenormalizedNode](t => generator.generateDenormalizedNode)
  implicit def osmDenormalizedNodesArb = Arbitrary { denormalizedNodeGenerator }
  
  def denormalizedWayGenerator = Gen.resultOf[Int, OsmDenormalizedWay](t => generator.generateDenormalizedWay)
  implicit def osmDenormalizedWaysArb = Arbitrary { denormalizedWayGenerator }
  
  def denormalizedRelationGenerator = Gen.resultOf[Int, OsmDenormalizedRelation](t => generator.generateDenormalizedRelation)
  implicit def osmDenormalizedRelationsArb = Arbitrary { denormalizedRelationGenerator }
  
  
  "The OsmDenormalizedSerializer 2" should {

    "serialize and deserialize an OsmDenormalizedNode object" ! forAll { osmDenormalizedNode: OsmDenormalizedNode =>
      {
        val serialized = toBinary(osmDenormalizedNode)
        val deserialized = fromBinary(serialized)
        deserialized must beSuccessfulTry(osmDenormalizedNode)
      }
    }

    "serialize and deserialize an OsmDenormalizedWay object" ! forAll { osmDenormalizedWay:OsmDenormalizedWay =>
    {
      val serialized = toBinary(osmDenormalizedWay)
      val deserialized = fromBinary(serialized)
      deserialized must beSuccessfulTry(osmDenormalizedWay)
    }
    }

    "serialize and deserialize an OsmDenormalizedRelation object" ! forAll { osmDenormalizedRelation: OsmDenormalizedRelation =>
      {
        val serialized = toBinary(osmDenormalizedRelation)
        val deserialized = fromBinary(serialized)
        deserialized must beSuccessfulTry(osmDenormalizedRelation)
      }
    }

  }

}

package io.plasmap.serializer.test

import io.plasmap.generator.OsmObjectGenerator
import io.plasmap.model._
import io.plasmap.serializer.OsmSerializer._
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification


class OsmSerializerTest extends Specification with ScalaCheck{

  sequential
  
  val generator = OsmObjectGenerator()
  
  def nodeGenerator = Gen.resultOf[Int,OsmNode](t => generator.generateNode)
  implicit def osmNodesArb = Arbitrary { nodeGenerator }
  
  def wayGenerator = Gen.resultOf[Int,OsmWay](t => generator.generateWay)
  implicit def osmWaysArb = Arbitrary { wayGenerator }
  
  def relationGenerator = Gen.resultOf[Int,OsmRelation](t => generator.generateRelation)
  implicit def osmRelationsArb = Arbitrary { relationGenerator }
  
  "The OsmSerializer 2" should {

    "serialize and deserialize an OsmNode object" ! forAll { osmNode: OsmNode =>
      {
        val serialized = toBinary(osmNode)
        val deserialized = fromBinary(serialized)
        deserialized must beSuccessfulTry(osmNode)
      }
    }
    
    "serialize and deserialize an OsmWay object" ! forAll { osmWay: OsmWay =>
      {
        val serialized = toBinary(osmWay)
        val deserialized = fromBinary(serialized)
        deserialized must beSuccessfulTry(osmWay)
      }
    }
    
    "serialize and deserialize an OsmRelation object" ! forAll { osmRelation: OsmRelation =>
      {
        val serialized = toBinary(osmRelation)
        val deserialized = fromBinary(serialized)
        deserialized must beSuccessfulTry(osmRelation)
      }
    }
  }
  

}

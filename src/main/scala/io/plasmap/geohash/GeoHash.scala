package io.plasmap.geohash

import io.plasmap.geohash.impl.GeoHashImpl
import io.plasmap.geohash.impl.GeoHashImpl.PRECISION

sealed abstract class Precision(val precision: PRECISION)

object PrecisionYottaLow_2BIT extends Precision(PRECISION.YOTTA_LOW_2BIT)
object PrecisionYottaLow_4BIT extends Precision(PRECISION.YOTTA_LOW_4BIT)
object PrecisionYottaLow_6BIT extends Precision(PRECISION.YOTTA_LOW_6BIT)

object PrecisionUltraLow_630KM extends Precision(PRECISION.ULTRA_LOW_8BIT)
object PrecisionUltraLow_10BIT extends Precision(PRECISION.ULTRA_LOW_10BIT)
object PrecisionUltraLow_12BIT extends Precision(PRECISION.ULTRA_LOW_12BIT)
object PrecisionUltraLow_14BIT extends Precision(PRECISION.ULTRA_LOW_14BIT)

object PrecisionVeryLow_80KM extends Precision(PRECISION.VERY_LOW_16BIT)
object PrecisionVeryLow_18BIT extends Precision(PRECISION.VERY_LOW_18BIT)

object PrecisionLow_20KM extends Precision(PRECISION.LOW_20BIT)
object PrecisionLow_22BIT extends Precision(PRECISION.LOW_22BIT)
object PrecisionLow_24BIT extends Precision(PRECISION.LOW_24BIT)
object PrecisionLow_26BIT extends Precision(PRECISION.LOW_26BIT)

object PrecisionMedium_5KM extends Precision(PRECISION.MEDIUM_28BIT)
object PrecisionMedium_30BIT extends Precision(PRECISION.MEDIUM_30BIT)
object PrecisionMedium_32BIT extends Precision(PRECISION.MEDIUM_32BIT)
object PrecisionMedium_34BIT extends Precision(PRECISION.MEDIUM_34BIT)

object PrecisionHigh_100M extends Precision(PRECISION.HIGH_36BIT)
object PrecisionHigh_38BIT extends Precision(PRECISION.HIGH_38BIT)
object PrecisionHigh_40BIT extends Precision(PRECISION.HIGH_40BIT)
object PrecisionHigh_42BIT extends Precision(PRECISION.HIGH_42BIT)
object PrecisionHigh_44BIT extends Precision(PRECISION.HIGH_44BIT)
object PrecisionHigh_46BIT extends Precision(PRECISION.HIGH_46BIT)

object PrecisionVeryHigh_1M extends Precision(PRECISION.VERY_HIGH_48BIT)
object PrecisionVeryHigh_50BIT extends Precision(PRECISION.VERY_HIGH_50BIT)
object PrecisionVeryHigh_52BIT extends Precision(PRECISION.VERY_HIGH_52BIT)
object PrecisionVeryHigh_54BIT extends Precision(PRECISION.VERY_HIGH_54BIT)
object PrecisionVeryHigh_56BIT extends Precision(PRECISION.VERY_HIGH_56BIT)
object PrecisionVeryHigh_58BIT extends Precision(PRECISION.VERY_HIGH_58BIT)

object PrecisionUltra_1CM extends Precision(PRECISION.ULTRA_60BIT)
object PrecisionUltra_62BIT extends Precision(PRECISION.ULTRA_62BIT)
object PrecisionUltraHigh_1MM extends Precision(PRECISION.ULTRA_HIGH_64BIT)

case class GeoHash(precision: Precision) {

  val geohash = new GeoHashImpl(precision.precision)

  def encodeParallel(lon: Double, lat: Double): Long = geohash.encodeParallel(lon, lat)

  def decodeParallel(hash: Long): (Double,Double) = {
    val decoded = geohash.decodeParallel(hash)
    (decoded(0),decoded(1))
  }

  def decodeSequential(hash: Long): (Double,Double) = {
    val decoded = geohash.decodeSequential(hash)
    (decoded(0),decoded(1))
  }

  def encodeSequential(lon: Double, lat: Double): Long = geohash.encodeSequential(lon, lat)

  def reduceParallelPrecision(hash: Long, reducedPrecision: Precision): Long = geohash.reducePrecisionParallel(hash, reducedPrecision.precision)

  def neighbourHashes(hash: Long): Tuple3[Tuple3[Long, Long, Long], Tuple3[Long, Long, Long], Tuple3[Long, Long, Long]] = {
    val N = geohash.getNeighbourHashes(hash)
    ((N(0)(0), N(0)(1), N(0)(2)),
      (N(1)(0), N(1)(1), N(1)(2)),
      (N(2)(0), N(2)(1), N(2)(2)))
  }

  def encapsulatingRectangle(hashes: List[Long]): Tuple2[Long, Long] = {
    import scala.collection.JavaConverters._
    val javaHashes: java.util.Collection[java.lang.Long] = hashes.map(hash => {
      val javaHash: java.lang.Long = hash
      javaHash
    }).asJavaCollection
    val rectangle = geohash.getEncapsulatingRectangle(new java.util.ArrayList(javaHashes))
    (rectangle(0), rectangle(1))
  }

  def encapsulatingRectangleHashes(upperLeft : Long, lowerRight : Long): Array[Array[Long]] = {
    geohash.getEncapsulatingRectangleBoundingBoxes(Array(upperLeft,lowerRight))
  }

}

object GeoHash{

  def yottaLow_6BIT= new GeoHash(PrecisionYottaLow_6BIT)
  def yottaLow_4BIT= new GeoHash(PrecisionYottaLow_4BIT)
  def yottaLow_2BIT= new GeoHash(PrecisionYottaLow_2BIT)

  def ultraLow = new GeoHash(PrecisionUltraLow_630KM)
  def ultraLow_10BIT = new GeoHash(PrecisionUltraLow_10BIT)
  def ultraLow_12BIT = new GeoHash(PrecisionUltraLow_12BIT)

  def veryLow = new GeoHash(PrecisionVeryLow_80KM)
  def veryLow_18BIT = new GeoHash(PrecisionVeryLow_18BIT)

  def low = new GeoHash(PrecisionLow_20KM)
  def low_22BIT = new GeoHash(PrecisionLow_22BIT)
  def low_24BIT = new GeoHash(PrecisionLow_24BIT)
  def low_26BIT = new GeoHash(PrecisionLow_26BIT)

  def medium = new GeoHash(PrecisionMedium_5KM)
  def medium_30BIT = new GeoHash(PrecisionMedium_30BIT)
  def medium_32BIT = new GeoHash(PrecisionMedium_32BIT)
  def medium_34BIT = new GeoHash(PrecisionMedium_34BIT)

  def high = new GeoHash(PrecisionHigh_100M)
  def high_38BIT = new GeoHash(PrecisionHigh_38BIT)
  def high_40BIT = new GeoHash(PrecisionHigh_40BIT)
  def high_42BIT = new GeoHash(PrecisionHigh_42BIT)
  def high_44BIT = new GeoHash(PrecisionHigh_44BIT)
  def high_46BIT = new GeoHash(PrecisionHigh_46BIT)

  def veryHigh = new GeoHash(PrecisionVeryHigh_1M)
  def veryHigh_50BIT = new GeoHash(PrecisionVeryHigh_50BIT)
  def veryHigh_52BIT = new GeoHash(PrecisionVeryHigh_52BIT)
  def veryHigh_54BIT = new GeoHash(PrecisionVeryHigh_54BIT)
  def veryHigh_56BIT = new GeoHash(PrecisionVeryHigh_56BIT)
  def veryHigh_58BIT = new GeoHash(PrecisionVeryHigh_58BIT)
  def ultra = new GeoHash(PrecisionUltra_1CM)
  def ultra_62 = new GeoHash(PrecisionUltra_62BIT)
  def ultraHigh = new GeoHash(PrecisionUltraHigh_1MM)

}
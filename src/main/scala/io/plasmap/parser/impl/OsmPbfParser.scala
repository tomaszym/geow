package io.plasmap.parser.impl

/**
 * Created by jm on 27/06/15.
 */

import io.plasmap.model.geometry.Point
import io.plasmap.model._
import io.plasmap.parser.OsmParser
import pbfbinaryparser.genclasses.Osmformat.{Relation, Node,DenseNodes, PrimitiveGroup, Way}
import pbfbinaryparser.genclasses.{Osmformat, Fileformat}
import java.util.zip.InflaterInputStream
import scala.annotation.tailrec
import scala.collection.{mutable}

import java.io.{DataInputStream, File, FileInputStream}


import scala.collection.JavaConverters._


case class OsmPbfParser (fileName: String)  extends OsmParser{
  val dis: DataInputStream = reachFirstBlob(fileName)
  val osmObjects = mutable.Queue[Option[OsmObject]]()

  override def hasNext(): Boolean ={
    dis.available() != 0 || osmObjects.nonEmpty
  }

  override def next(): Option[OsmObject] = {
    if( osmObjects.isEmpty )
      repopulateQueue(osmObjects, dis)

    osmObjects.dequeue()
  }

  def reachFirstBlob(fileName: String) : DataInputStream = {
    val dis = new DataInputStream(new FileInputStream(new File(fileName)))
    val bhSize: Int = dis.readInt
    val rawBH: Array[Byte] = new Array[Byte](bhSize)
    dis.read(rawBH)
    var bh: Fileformat.BlobHeader = Fileformat.BlobHeader.parseFrom(rawBH)
    val blobSize = bh.getDatasize

    val rawBlob = new Array[Byte](blobSize)
    dis.read(rawBlob)
    val blob: Fileformat.Blob = Fileformat.Blob.parseFrom(rawBlob)

    val hb: Osmformat.HeaderBlock = Osmformat.HeaderBlock.parseFrom(blob.getRaw)
    dis
  }

  def nextBlob(dis : DataInputStream): Fileformat.Blob = {

    val bhSize = dis.readInt()
    val rawBH = new Array[Byte](bhSize)

    dis.read(rawBH)

    val bh = Fileformat.BlobHeader.parseFrom(rawBH)
    val blobSize = bh.getDatasize
    val rawBlob = new Array[Byte](blobSize)

    dis.read(rawBlob)
    Fileformat.Blob.parseFrom(rawBlob)
  }

  def repopulateQueue(queue: mutable.Queue[Option[OsmObject]], dis: DataInputStream): Unit = {
    val pb = uncompressBlob( nextBlob(dis) )

    //The string table contains all the string of the blob
    val stringTable = pb.getStringtable.getSList.asScala.map(_.toStringUtf8).toArray

    //Necessary to compute a geographical data from the raw lat and lon values of the files + same for the date
    val forTheMaths = Map(
      "latOffset" -> pb.getLatOffset,
      "lonOffset" -> pb.getLonOffset,
      "geoGranularity" -> pb.getGranularity.toLong,
      "dateGranularity" -> pb.getDateGranularity.toLong)


    val primitiveGroups = pb.getPrimitivegroupList.asScala

    primitiveGroups.foreach(
      (pg :PrimitiveGroup) => {

        getDenseNodes(stringTable, forTheMaths, pg.getDense)
          .foreach( n => osmObjects.enqueue(Option(n)) )

        pg.getNodesList.asScala
          .map( getSingleNode(stringTable, forTheMaths, _) )
          .foreach( n => osmObjects.enqueue(Option(n)))

        pg.getRelationsList.asScala
          .map( getRelations(stringTable, _) )
          .foreach( r => osmObjects.enqueue(Option(r)) )

        pg.getWaysList.asScala
          .map( getWays(stringTable , _) )
          .foreach( w => osmObjects.enqueue(Option(w)) )
      }
    )
  }

  def uncompressBlob( blob: Fileformat.Blob): Osmformat.PrimitiveBlock = {
      val decompresser = new InflaterInputStream (blob.getZlibData ().newInput () )
      val pb = Osmformat.PrimitiveBlock.parseFrom (decompresser)
      decompresser.close ()
      pb
  }

  def getWays(stringTable: Array[String], way: Way): OsmWay = {
    val id = new OsmId( way.getId )
    val osmTags = List(
      way.getKeysList.asScala,
      way.getValsList.asScala
    )
      .transpose
      .map( t  => new OsmTag(stringTable(t(0).asInstanceOf[Int] ), stringTable(t(1).asInstanceOf[Int]) ) )

    val osmRefs = valFromDelta( way.getRefsList.asScala.toList ).map(new OsmId(_))
    val user = new OsmUser( stringTable( way.getInfo.getUserSid ), way.getInfo.getUid)
    val version = way.getInfo.getVersion

    new OsmWay( id, Option(user), new OsmVersion(version), osmTags, osmRefs)
  }

  def getRelations(stringTable: Array[String], relation: Relation): OsmRelation = {
    val osmId = new OsmId(relation.getId)
    val osmUser = new OsmUser( stringTable( relation.getInfo.getUserSid ), relation.getInfo.getUid )
    val osmVersion = new OsmVersion(  relation.getInfo.getVersion )
    val osmTags =
      List( relation.getKeysList.asScala.toList, relation.getValsList.asScala )
      .transpose
      .map( t  => new OsmTag(stringTable(t(0).asInstanceOf[Int] ), stringTable(t(1).asInstanceOf[Int]) ) )

    def getOsmType( sType : Int ): OsmType = stringTable(sType) match {
      case "way"      => OsmTypeWay
      case "relation" => OsmTypeRelation
      case _ => OsmTypeNode
    }

    def getOsmRole( sRole : Int ) : OsmRole = stringTable(sRole) match {
      case "inner" => OsmRoleInner
      case "outer" => OsmRoleOuter
      case ""      => OsmRoleEmpty
      case r       => new OsmRoleOther(r)
    }
    val osmMembers = List(
      relation.getTypesList.asScala.map( x => new java.lang.Integer(x.getNumber) ),
      valFromDelta( relation.getMemidsList.asScala.toList ),
      relation.getRolesSidList.asScala
    )
      .transpose
      .map( m => new OsmMember(
          getOsmType(m(0).asInstanceOf[Integer]),
          new OsmId(m(1).asInstanceOf[Long]),
          getOsmRole( m(2).asInstanceOf[Int])
        )
      )

    new OsmRelation(osmId, Option(osmUser), osmVersion, osmTags, osmMembers)
  }

  def getSingleNode(stringTable: Array[String], forTheMaths: Map[String,Long], node: Node): OsmNode = {
    val osmId = new OsmId(node.getId)
    val osmUser = new OsmUser( stringTable( node.getInfo.getUserSid ), node.getInfo.getUid )
    val osmVersion = new OsmVersion(  node.getInfo.getVersion )

    val osmTags =
      List( valFromDelta(node.getKeysList.asScala.toList), valFromDelta(node.getValsList.asScala.toList) )
      .transpose
      .map( t  => new OsmTag(stringTable(t(0).asInstanceOf[Int] ), stringTable(t(1).asInstanceOf[Int]) ) )

    val latOffset = forTheMaths("latOffset")
    val lonOffset = forTheMaths("lonOffset")
    val geoGran = forTheMaths("geoGranularity")

    val lat = node.getLat
    val lon = node.getLon
    val geoPoint = Point(
      0.000000001 * (latOffset + (geoGran * lat)),
      0.000000001 * (lonOffset + (geoGran * lon))
    )

    new OsmNode(osmId, Option(osmUser), osmVersion, osmTags, geoPoint )
  }

  def getDenseNodes( stringTable : Array[String], forTheMaths: Map[String,Long], dns : DenseNodes): List[OsmNode] ={
    val valIds = valFromDelta(dns.getIdList.asScala.toList)
    val tags = getTagsNodeIds(dns.getKeysValsList.asScala.toList, valIds)

    val latOffset = forTheMaths("latOffset")
    val lonOffset = forTheMaths("lonOffset")
    val geoGran = forTheMaths("geoGranularity")
    val dateGran = forTheMaths("dateGranularity")

    val grouped: List[(Long, List[(Integer, Integer, Long)])] =
      tags
        .groupBy({case (key, v, id) => id})
        .toList
        .sortWith( (x,y) => x._1.compareTo(y._1) < 0 )

    val nodes = List(
      grouped,
      valFromDelta(dns.getLatList.asScala.toList).map( (lat : Long) => 0.000000001 * (latOffset + (geoGran * lat))),
      valFromDelta(dns.getLonList.asScala.toList).map( (lon : Long) => 0.000000001 * (lonOffset + (geoGran * lon))),
      valFromDelta(dns.getDenseinfo.getChangesetList.asScala.toList),
      valFromDelta(dns.getDenseinfo.getTimestampList.asScala.toList).map( (ts : Long) => ts * dateGran),
      valFromDelta(dns.getDenseinfo.getUidList.asScala.toList),
      valFromDelta(dns.getDenseinfo.getUserSidList.asScala.toList),
      dns.getDenseinfo.getVersionList.asScala.map(_.longValue())
    ).transpose

    val osmNodes = nodes.map(n => { toOsmNode(
        n(0).asInstanceOf[(Long, List[(Integer, Integer, Long)])]._1,
        n(0).asInstanceOf[(Long, List[(Integer, Integer, Long)])]._2.map( (kvid : (Integer,Integer,Long)) => (stringTable(kvid._1), stringTable(kvid._2), kvid._3)),
        n(1).asInstanceOf[Double],
        n(2).asInstanceOf[Double],
        n(3).asInstanceOf[Long],
        n(4).asInstanceOf[Long],
        n(5).asInstanceOf[Long],
        stringTable( n(6).asInstanceOf[Long].toInt ),
        n(7).asInstanceOf[Long])
    })
    osmNodes
  }

  def toOsmNode( id : Long, tags :List[(String,String,Long)], lon : Double, lat: Double, changeSet: Long, timeStamp: Long, uid : Long, user :String, version: Long): OsmNode = {
    val osmUser = new OsmUser(user, uid)
    val osmTags = tags.filter(_._1 != None).map( {case (key : String, v :String, id :Long) => new OsmTag(key, v)})
    val osmId = new OsmId(id)
    val point = Point(lon, lat)

    new OsmNode(osmId, Option(osmUser),new OsmVersion(version),osmTags,point)

  }

  def valFromDelta(l : List[java.lang.Number]): List[Long] = {
    @tailrec def go( l : List[java.lang.Number] , delta : Long, acc : List[Long]):  List[Long] = l match {
      case Nil => acc
      case h :: t => go( t, h.longValue()+ delta, (h.longValue() + delta) :: acc)
    }

    go(l, 0, List.empty[Long]).reverse
  }
  /*
   * Two succeeding integers in the key value list kv represent the key=value pair of a single <tag> element.
   * The value zero in the kv list represent the move to a new node that can contains several tags or none.
   */
  def getTagsNodeIds( kv : List[Integer], ids: List[Long]): List[(Integer, Integer, Long)] = {
    @tailrec def go(kv : List[Integer], ids: List[Long], acc :List[(Integer, Integer, Long)]) : List[(Integer, Integer, Long)] = ids.size match {
      case 0 => acc
      case _ => kv match {
        case (h :: t) if h == 0 => go(t, ids.tail,  (new Integer(0), new Integer(0), ids.head) :: acc)
        case (h :: t) if h != 0 => go(t.tail, ids,  (h, t.head, ids.head) :: acc)
      }

    }

    go(kv, ids, List.empty[Tuple3[Integer, Integer, Long]])
  }

}

object OsmPbfParser{
  def main (args: Array[String]) {
    val f = "/home/jm/Documents/programmation/geow-org/sample.pbf"
    val parser = new OsmPbfParser(new File(f))

    parser.foreach( println _ )
  }
}

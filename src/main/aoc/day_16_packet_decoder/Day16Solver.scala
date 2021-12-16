package aoc.day_16_packet_decoder

import aoc.file_loader.AOCLoader

case class Packet(version: Int, tid: Int, value: BigInt, nested: Array[Packet], isLong: Boolean)

object Day16Solver {

  def main(args: Array[String]): Unit = {
    val lines = AOCLoader.loadActual("day_16_packet_decoder")
//    val lines = AOCLoader.loadExample("day_16_packet_decoder")
    val bits = lines.head.map(c => BigInt(c.toString, 16).toString(2))
      .flatMap(s => String.format("%4s", s).replace(" ", "0")).mkString

    println(solvePart1(bits))
    println(solvePart2(bits))
  }

  def getFewBitAsInt(bits: String, start: Int, count: Int): Int = {
    Integer.parseInt(bits.substring(start, start + count), 2)
  }

  def parseValues(bits: String, start: Int): (BigInt, Int) = {
    var cur = start
    var value = BigInt(0)
    var end = false
    while (!end) {
      val cbs = bits.substring(cur + 1, cur + 5)
      value = (value << 4) + BigInt(cbs, 2)
      end = bits(cur) == '0'
      cur += 5
    }
    (value, cur)
  }

  def makePacket(bits: String, start: Int, end: Int, max: Int): (Array[Packet], Int) = {
    var packets = Array[Packet]()
    var cur = start
    var i = 0

    while (cur + 11 <= end && i < max) {
      val version = getFewBitAsInt(bits, cur, 3)
      val tid = getFewBitAsInt(bits, cur + 3, 3)
      cur = cur + 6
      if (tid == 4) {
        val vcp = parseValues(bits, cur)
        cur = vcp._2
        packets = packets :+ Packet(version, tid, vcp._1, Array.empty, isLong = false)
      } else {
        val isLong = bits(cur) == '0'
        var pkc = 9999999
        var pke = end
        cur = cur + 1
        if (isLong) {
          pke = getFewBitAsInt(bits, cur, 15) + cur + 15
          cur += 15
        }
        else {
          pkc = getFewBitAsInt(bits, cur, 11)
          cur += 11
        }

        val mkpc = makePacket(bits, cur, pke, pkc)
        cur = mkpc._2
        packets = packets :+ Packet(version, tid, 0, mkpc._1, isLong = isLong)
      }
      i += 1
    }

    (packets, cur)
  }

  def sumPacketVersion(packet: Packet): BigInt = {
    packet.nested.map(sumPacketVersion).sum + packet.version
  }

  def solvePart1(bits: String): BigInt = {
    makePacket(bits, 0, bits.length, 1)._1.map(sumPacketVersion).sum
  }

  def boolToInt(boolean: Boolean): Int = {
    if (boolean) 1 else 0
  }

  def solvePacket(packet: Packet): BigInt = {
    packet.tid match {
      case 0 => packet.nested.map(solvePacket).sum
      case 1 => packet.nested.map(solvePacket).product
      case 2 => packet.nested.map(solvePacket).min
      case 3 => packet.nested.map(solvePacket).max
      case 4 => packet.value
      case 5 => boolToInt(solvePacket(packet.nested(0)) > solvePacket(packet.nested(1)))
      case 6 => boolToInt(solvePacket(packet.nested(0)) < solvePacket(packet.nested(1)))
      case 7 => boolToInt(solvePacket(packet.nested(0)) == solvePacket(packet.nested(1)))
    }
  }

  def solvePart2(bits: String): BigInt = {
    solvePacket(makePacket(bits, 0, bits.length, 1)._1.head)
  }
}

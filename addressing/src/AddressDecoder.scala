// See LICENSE.SiFive for license details.

package org.chipsalliance.utils.addressing

import chisel3.util.log2Ceil
import logger.LazyLogging

object AddressDecoder extends LazyLogging {
  type Port = Seq[AddressSet]
  type Ports = Seq[Port]
  type Partition = Ports
  type Partitions = Seq[Partition]

  val addressOrder:   Ordering[AddressSet] = Ordering.ordered[AddressSet]
  val portOrder:      Ordering[Iterable[AddressSet]] = Ordering.Iterable(addressOrder)
  val partitionOrder: Ordering[Iterable[Iterable[AddressSet]]] = Ordering.Iterable(portOrder)

  // Find the minimum subset of bits needed to disambiguate port addresses.
  // ie: inspecting only the bits in the output, you can look at an address
  //     and decide to which port (outer Seq) the address belongs.
  def apply(ports: Ports, givenBits: BigInt = BigInt(0)): BigInt = {
    val nonEmptyPorts = ports.filter(_.nonEmpty)
    if (nonEmptyPorts.size <= 1) {
      givenBits
    } else {
      // Verify the user did not give us an impossible problem
      nonEmptyPorts.combinations(2).foreach {
        case Seq(x, y) =>
          x.foreach { a =>
            y.foreach { b =>
              require(!a.overlaps(b), s"Ports cannot overlap: $a $b")
            }
          }
      }

      val maxBits = log2Ceil(1 + nonEmptyPorts.map(_.map(_.base).max).max)
      val (bitsToTry, bitsToTake) = (0 until maxBits)
        .map(BigInt(1) << _)
        .partition(b => (givenBits & b) == 0)
      val partitions = Seq(nonEmptyPorts.map(_.sorted).sorted(portOrder))
      val givenPartitions = bitsToTake.foldLeft(partitions) { (p, b) =>
        partitionPartitions(p, b)
      }
      val selected = recurse(givenPartitions, bitsToTry.reverse)
      val output = selected.reduceLeft(_ | _) | givenBits

      // Modify the AddressSets to allow the new wider match functions
      val widePorts = nonEmptyPorts.map {
        _.map {
          _.widen(~output)
        }
      }
      // Verify that it remains possible to disambiguate all ports
      widePorts.combinations(2).foreach {
        case Seq(x, y) =>
          x.foreach { a =>
            y.foreach { b =>
              require(!a.overlaps(b), s"Ports cannot overlap: $a $b")
            }
          }
      }

      output
    }
  }

  // A simpler version that works for a Seq[Int]
  def apply(keys: Seq[Int]): Int = {
    val ports = keys.map(b => Seq(AddressSet(b, 0)))
    apply(ports).toInt
  }

  // The algorithm has a set of partitions, discriminated by the selected bits.
  // Each partition has a set of ports, listing all addresses that lead to that port.
  // Seq[Seq[Seq[AddressSet]]]
  //         ^^^^^^^^^^^^^^^ set of addresses that are routed out this port
  //     ^^^ the list of ports
  // ^^^ cases already distinguished by the selected bits thus far
  //
  // Solving this problem is NP-hard, so we use a simple greedy heuristic:
  //   pick the bit which minimizes the number of ports in each partition
  //   as a secondary goal, reduce the number of AddressSets within a partition

  def bitScore(partitions: Partitions): Seq[Int] = {
    val maxPortsPerPartition = partitions.map(_.size).max
    val maxSetsPerPartition = partitions.map(_.map(_.size).sum).max
    val sumSquarePortsPerPartition = partitions.map(p => p.size * p.size).sum
    val sumSquareSetsPerPartition =
      partitions.map(_.map(p => p.size * p.size).sum).max
    Seq(
      maxPortsPerPartition,
      maxSetsPerPartition,
      sumSquarePortsPerPartition,
      sumSquareSetsPerPartition
    )
  }

  def partitionPort(port: Port, bit: BigInt): (Port, Port) = {
    val addr_a = AddressSet(0, ~bit)
    val addr_b = AddressSet(bit, ~bit)
    // The addresses were sorted, so the filtered addresses are still sorted
    val subset_a = port.filter(_.overlaps(addr_a))
    val subset_b = port.filter(_.overlaps(addr_b))
    (subset_a, subset_b)
  }

  def partitionPorts(ports: Ports, bit: BigInt): (Ports, Ports) = {
    val partitioned_ports = ports.map(p => partitionPort(p, bit))
    // because partitionPort dropped AddresSets, the ports might no longer be sorted
    val case_a_ports =
      partitioned_ports.map(_._1).filter(_.nonEmpty).sorted(portOrder)
    val case_b_ports =
      partitioned_ports.map(_._2).filter(_.nonEmpty).sorted(portOrder)
    (case_a_ports, case_b_ports)
  }

  def partitionPartitions(partitions: Partitions, bit: BigInt): Partitions = {
    val partitioned_partitions = partitions.map(p => partitionPorts(p, bit))
    val case_a_partitions = partitioned_partitions.map(_._1).filter(_.nonEmpty)
    val case_b_partitions = partitioned_partitions.map(_._2).filter(_.nonEmpty)
    val new_partitions =
      (case_a_partitions ++ case_b_partitions).sorted(partitionOrder)
    // Prevent combinational memory explosion; if two partitions are equal, keep only one
    // Note: AddressSets in a port are sorted, and ports in a partition are sorted.
    // This makes it easy to structurally compare two partitions for equality
    val keep = new_partitions.init
      .zip(new_partitions.tail)
      .filter {
        case (a, b) => partitionOrder.compare(a, b) != 0
      }
      .map {
        _._2
      }
    new_partitions.head +: keep
  }

  // requirement: ports have sorted addresses and are sorted lexicographically
  def recurse(partitions: Partitions, bits: Seq[BigInt]): Seq[BigInt] = {
    if (partitions.map(_.size <= 1).reduce(_ && _)) Seq()
    else {
      logger.debug("Partitioning:\n")
      partitions.foreach { partition =>
        logger.debug("  Partition:\n")
        partition.foreach { port =>
          logger.debug("   ")
          port.foreach { a =>
            logger.debug(s" $a")
          }
          logger.debug("")
        }
      }
      val candidates = bits.map { bit =>
        val result = partitionPartitions(partitions, bit)
        val score = bitScore(result)
        logger.debug("  For bit %x, %s\n".format(bit, score.toString))
        (score, bit, result)
      }
      val (bestScore, bestBit, bestPartitions) = candidates.min(
        Ordering
          .by[(Seq[Int], BigInt, Partitions), Iterable[Int]](_._1.toIterable)
      )
      logger.debug("=> Selected bit 0x%x".format(bestBit))
      bestBit +: recurse(bestPartitions, bits.filter(_ != bestBit))
    }
  }
}

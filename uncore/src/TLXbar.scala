package uncore.tilelink

import chisel3._
import chisel3.util.{log2Ceil, DecoupledIO, Mux1H}
import diplomacy.{LazyModule, LazyModuleImp}
import logger.LazyLogging
import org.chipsalliance.utils.addressing.{AddressDecoder, AddressSet, IdRange, RegionType}
import org.chipsalliance.utils.misc.EnhancedChisel3Assign
import tilelink.{
  ChannelAMessage,
  ChannelBMessage,
  TLBundle,
  TLBundleParameters,
  TLChannel,
  TLDecoupledNexusNode,
  TLMasterParameters,
  TLMasterPortParameters,
  TLSlaveParameters,
  TLSlavePortParameters
}
import pprint.{pprintln => println}

object TLXbar {
  private def assignRanges(sizes: Seq[Int]): Seq[IdRange] = {
    val pow2Sizes = sizes.map { z => if (z == 0) 0 else 1 << log2Ceil(z) }
    // record old index, then sort by increasing size
    val tuples = pow2Sizes.zipWithIndex.sortBy(_._1)
    // suffix-sum of the sizes = the start positions
    val starts = tuples.scanRight(0)(_._1 + _).tail
    val ranges = (tuples.zip(starts)).map {
      case ((sz, i), st) =>
        (if (sz == 0) IdRange(0, 0) else IdRange(st, st + sz), i)
    }
    // Restore original order
    ranges.sortBy(_._2).map(_._1)
  }

  def mapInputIds(ports: Seq[TLMasterPortParameters]): Seq[IdRange] = assignRanges(ports.map(_.endSourceId))

  def mapOutputIds(ports: Seq[TLSlavePortParameters]): Seq[IdRange] = assignRanges(ports.map(_.endSinkId))

  def relabeler(): () => Int => Int = {
    var idFactory = 0
    () => {
      val fifoMap = scala.collection.mutable.HashMap.empty[Int, Int]
      (x: Int) => {
        if (fifoMap.contains(x)) fifoMap(x)
        else {
          val out = idFactory
          idFactory = idFactory + 1
          fifoMap += (x -> out)
          out
        }
      }
    }
  }

  /** Replicate an input port to each output port. */
  def fanout[T <: TLChannel](input: DecoupledIO[T], select: Seq[Bool]): Seq[DecoupledIO[T]] = {
    val filtered = Wire(Vec(select.size, chiselTypeOf(input)))
    for (i <- select.indices) {
      filtered(i).bits := input.bits
      filtered(i).valid := input.valid
    }
    input.ready := Mux1H(select, filtered.map(_.ready))
    filtered
  }
}

class TLXbar(policy: TLArbiter.Policy = TLArbiter.roundRobin) extends LazyModule with LazyLogging {
  val node = new TLDecoupledNexusNode(
    clientFn = { seq =>
      seq.head.copy(
        masters = TLXbar.mapInputIds(seq).zip(seq).flatMap {
          case (range, port) =>
            port.masters.map { client =>
              client.copy(
                sourceId = client.sourceId.shift(range.start)
              )
            }
        }
      )
    },
    managerFn = { seq =>
      val fifoDomainFactory = TLXbar.relabeler()
      seq.head.copy(
        endSinkId = TLXbar.mapOutputIds(seq).map(_.end).max,
        slaves = seq.flatMap { port =>
          require(
            port.channelBeatBytes == seq.head.channelBeatBytes,
            s"""Xbar ($name with parent $parent) data widths don't match: 
               |${port.slaves.map(_.name)} has ${port.channelBeatBytes}B
               |${seq.head.slaves.map(_.name)} has ${seq.head.channelBeatBytes}B""".stripMargin
          )
          val fifoDomainMapper = fifoDomainFactory()
          port.slaves.map { manager =>
            manager.copy(
              fifoDomain = manager.fifoDomain.map(fifoDomainMapper(_))
            )
          }
        }
      )
    }
  ) {
    override def circuitIdentity: Boolean = outputs.size == 1 && inputs.size == 1
  }
  logger.error(s"constructed ${pprint.apply(node)}")

  lazy val module = new LazyModuleImp(this) {
    def transpose[T](x: Seq[Seq[T]]) =
      if (x.isEmpty) Nil else Vector.tabulate(x.head.size) { i => Vector.tabulate(x.size) { j => x(j)(i) } }

    def unique(x: Vector[Boolean]) = (x.count(x => x) <= 1).B

    // Interface
    val (inIO, edgesIn) = node.in.unzip
    val (outIO, edgesOut) = node.out.unzip

    logger.debug(s"Generating TLXBar $pathName with ${node.in.size} Clients and ${node.out.size} Managers.")
    /* Not every master need connect to every slave on every channel
     * determine which connections are necessary
     * connectivity of a channel depends on:
     *   1. whether can a client node can observe the address in the manager.
     *   1. client can support transaction that emit by a manager on that channel.
     *   1. manager can support transaction that emit by a client on that channel.
     */

    /** whether can a client node can observe the address in a manager. */
    val reachableIO =
      edgesIn.map { edgeIn =>
        edgesOut.map { edgeOut =>
          edgeIn.masterPortParameters.masters.exists { clientParameters =>
            edgeOut.slavePortParameters.slaves.exists { managerParameters =>
              // whether can a client node can observe the address in the manager.
              clientParameters.visibility.exists { ca =>
                managerParameters.address.exists { ma =>
                  ca.overlaps(ma)
                }
              }
            }
          }
        }.toVector
      }.toVector

    /** whether can a client port can observe manager port. */
    def visible(clientPortParameters: TLMasterPortParameters, managerPortParameters: TLSlavePortParameters): Boolean =
      clientPortParameters.masters.exists { clientParameters =>
        managerPortParameters.slaves.exists { managerParameters =>
          clientParameters.visibility.exists { ca =>
            managerParam

            eters.address.exists { ma =>
              ca.overlaps(ma)
            }
          }
        }
      }

    def clientSupportManager(
      clientPortParameters:  TLMasterPortParameters,
      managerPortParameters: TLSlavePortParameters
    ) =
      clientPortParameters.masters.map { clientParameters =>
        managerPortParameters.slaves.map { managerParameters =>
          managerParameters.emits.map { emit =>
            clientParameters.supports
              .map(s => s.support(emit))
              // exist a client can support emit from manager
              .reduce(_ || _)
          }
          // all emit should be supported
            .reduce(_ && _)
        }
      }

    def managerSupportClient(
      clientPortParameters:  TLMasterPortParameters,
      managerPortParameters: TLSlavePortParameters
    ) =
      clientPortParameters.masters.map { clientParameters =>
        managerPortParameters.slaves.map { managerParameters =>
          clientParameters.emits.map { emit =>
            managerParameters.supports
              .map(s => s.support(emit))
              // exist a manager can support emit from manager
              .reduce(_ || _)
          }
          // all emit should be supported
            .reduce(_ && _)
        }
      }

    val connectAIO =
      edgesIn.map { edgeIn =>
        edgesOut.map { edgeOut =>
          val clientPortParameters = edgeIn.masterPortParameters
          val managerPortParameters = edgeOut.slavePortParameters
          visible(clientPortParameters, managerPortParameters)
        }
      }

    val connectDIO = reachableIO
    val connectAOI = transpose(connectAIO)
    val connectDOI = transpose(connectDIO)

    /** whether can a client node can be probed by a manager.
      * Not only need reachable, but also the client node supports probe and manager node can store
      */
    val probeIO = (edgesIn
      .zip(reachableIO))
      .map {
        case (edgeIn, reachableO) =>
          (edgesOut
            .zip(reachableO))
            .map {
              case (mp, reachable) =>
                reachable &&
                  // client can connect to manager
                  edgeIn.masterPortParameters.masters
                    .flatMap(_.supports.filter(_.isInstanceOf[ChannelBMessage]))
                    .nonEmpty &&
                  mp.slavePortParameters.slaves.exists(_.regionType >= RegionType.TRACKED)
            }
            .toVector
      }
      .toVector
    val releaseIO = (edgesIn
      .zip(reachableIO))
      .map {
        case (cp, reachableO) =>
          (edgesOut
            .zip(reachableO))
            .map {
              case (mp, reachable) =>
                reachable && cp.masterPortParameters.anySupportProbe && mp.slavePortParameters.anySupportAcquiredB
            }
            .toVector
      }
      .toVector
    val connectBIO = probeIO
    val connectBOI = transpose(connectBIO)
    val connectCIO = releaseIO
    val connectCOI = transpose(connectCIO)
    val connectEIO = releaseIO
    val connectEOI = transpose(connectEIO)

    logger.error(s"""
                    |matrix:
                    |A: ${connectAIO}
                    |D: ${connectAIO}
                    |
                    |B: ${connectBIO}
                    |C: ${connectCIO}
                    |E: ${connectEIO}
                    |""".stripMargin)

    // Grab the port ID mapping
    val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.masterPortParameters))
    val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.slavePortParameters))

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = TLBundleParameters.union(inIO.map(_.bundleParameters) ++ outIO.map(_.bundleParameters))

    // Handle size = 1 gracefully (Chisel3 empty range is broken)
    def trim(id: UInt, size: Int) = if (size <= 1) 0.U else id(log2Ceil(size) - 1, 0)

    // Transform input bundle sources (sinks use global namespace on both sides)
    val in = Wire(Vec(inIO.size, TLBundle.decoupled(wide_bundle)))

    // Transform output bundle sinks (sources use global namespace on both sides)
    val out = Wire(Vec(outIO.size, TLBundle.decoupled(wide_bundle)))

    // Filter a list to only those elements selected
    def filter[T](data: Seq[T], mask: Seq[Boolean]) = (data.zip(mask)).filter(_._2).map(_._1)

    // Based on input=>output connectivity, create per-input minimal address decode circuits
    val requiredAC = (connectAIO ++ connectCIO).distinct
    val outputPortFns: Map[Vector[Boolean], Seq[UInt => Bool]] = requiredAC.map { connectO =>
      val port_addrs = edgesOut.map(_.slavePortParameters.slaves.flatMap(_.address))
      val routingMask = AddressDecoder(filter(port_addrs, connectO))
      val route_addrs = port_addrs.map(seq => AddressSet.unify(seq.map(_.widen(~routingMask)).distinct))

      logger.trace("Xbar mapping:\n")
      route_addrs.foreach { p =>
        p.foreach { a => logger.trace(s" ${a}") }
        logger.trace("\n")
      }

      (connectO, route_addrs.map(seq => (addr: UInt) => seq.map(_.contains(addr)).reduce(_ || _)))
    }.toMap

    // Print the ID mapping
    edgesIn.zip(inputIdRanges).zipWithIndex.foreach {
      case ((edge, id), i) =>
        logger.trace(s"\t$i assigned $id for ${edge.masterPortParameters.masters.map(_.name).mkString(", ")}\n")
    }

    in.indices.map { i =>
      val r = inputIdRanges(i)

      if (connectAIO(i).exists(x => x)) {
        in(i).a :<> inIO(i).a
        in(i).a.bits.source := inIO(i).a.bits.source | r.start.U
      } else {
        in(i).a.valid := false.B
        inIO(i).a.ready := true.B
      }

      if (connectDIO(i).exists(x => x)) {
        inIO(i).d :<> in(i).d
        inIO(i).d.bits.source := trim(in(i).d.bits.source, r.size)
      } else {
        in(i).d.ready := true.B
        inIO(i).d.valid := false.B
      }
    }

    out.indices.map { o =>
      val r = outputIdRanges(o)

      if (connectAOI(o).exists(x => x)) {
        outIO(o).a :<> out(o).a
      } else {
        out(o).a.ready := true.B
        outIO(o).a.valid := false.B
      }

      if (connectDOI(o).exists(x => x)) {
        out(o).d :<> outIO(o).d
        out(o).d.bits.sink := outIO(o).d.bits.sink | r.start.U
      } else {
        out(o).d.valid := false.B
        outIO(o).d.ready := true.B
      }

    }

    val beatsAI = in.zip(edgesIn).map { case (i, e) => e.numBeats1(i.a.bits) }
    val beatsDO = out.zip(edgesOut).map { case (o, e) => e.numBeats1(o.d.bits) }

    val addressA = (in.zip(edgesIn)).map { case (i, e) => e.address(i.a.bits) }
    val requestAIO = (connectAIO.zip(addressA)).map { case (c, i) => outputPortFns(c).map { o => unique(c) || o(i) } }
    val requestDOI = out.map { o => inputIdRanges.map { i => i.contains(o.d.bits.source) } }

    // Fanout the input sources to the output sinks
    val portsAOI = transpose(
      in.zip(requestAIO).map { case (i, r) => TLXbar.fanout(i.a, r) }
    )
    val portsDIO = transpose(
      out.zip(requestDOI).map { case (o, r) => TLXbar.fanout(o.d, r) }
    )

    out.indices.map { o =>
      TLArbiter(policy)(out(o).a, filter(beatsAI.zip(portsAOI(o)), connectAOI(o)): _*)
    }

    in.indices.map { i =>
      TLArbiter(policy)(in(i).d, filter(beatsDO.zip(portsDIO(i)), connectDIO(i)): _*)
    }

    // TL-C

    in.indices.map { i =>
      val r = inputIdRanges(i)
      if (edgesIn(i).bundleParameters.isTLC) {
        if (connectBIO(i).exists(x => x)) {
          inIO(i).b :<> in(i).b
          inIO(i).b.bits.source := trim(in(i).b.bits.source, r.size)
        } else {
          in(i).b.ready := true.B
          inIO(i).b.valid := false.B
        }
        if (connectCIO(i).exists(x => x)) {
          in(i).c :<> inIO(i).c
          in(i).c.bits.source := inIO(i).c.bits.source | r.start.U
        } else {
          in(i).c.valid := false.B
          inIO(i).c.ready := true.B
        }

        if (connectEIO(i).exists(x => x)) {
          in(i).e :<> inIO(i).e
        } else {
          if (edgesIn(i).bundleParameters.isTLC) {
            in(i).e.valid := false.B
            inIO(i).e.ready := true.B
          }
        }
      }
    }

    out.indices.map { o =>
      val r = outputIdRanges(o)
      if (edgesOut(o).bundleParameters.isTLC) {
        if (connectBOI(o).exists(x => x)) {
          out(o).b :<> outIO(o).b
        } else {
          if (edgesOut(o).bundleParameters.isTLC) {
            out(o).b.valid := false.B
            outIO(o).b.ready := true.B
          }
        }

        if (connectCOI(o).exists(x => x)) {
          outIO(o).c :<> out(o).c
        } else {
          if (edgesOut(o).bundleParameters.isTLC) {
            out(o).c.ready := true.B
            outIO(o).c.valid := false.B
          }
        }
        if (connectEOI(o).exists(x => x)) {
          outIO(o).e :<> out(o).e
          outIO(o).e.bits.sink := trim(out(o).e.bits.sink, r.size)
        } else {
          if (edgesOut(o).bundleParameters.isTLC) {
            out(o).e.ready := true.B
            outIO(o).e.valid := false.B
          }
        }
      }
    }

    val inC = in.filter(_.bundleParameters.isTLC)
    val edgesInC = edgesIn.filter(_.bundleParameters.isTLC)
    val outC = in.filter(_.bundleParameters.isTLC)
    val edgesOutC = edgesOut.filter(_.bundleParameters.isTLC)
    val inputIdRangesC = TLXbar.mapInputIds(edgesInC.map(_.masterPortParameters))
    val outputIdRangesC = TLXbar.mapOutputIds(edgesOutC.map(_.slavePortParameters))

    val portsBIO = transpose(outC.zip(outC.map { o => inputIdRangesC.map { i => i.contains(o.b.bits.source) } }).map {
      case (o, r) => TLXbar.fanout(o.b, r)
    })
    val portsCOI = transpose(
      inC
        .zip(connectCIO.zip(inC.zip(edgesInC).map { case (i, e) => e.address(i.c.bits) }).map {
          case (c, i) => outputPortFns(c).map { o => unique(c) || o(i) }
        })
        .map { case (i, r) => TLXbar.fanout(i.c, r) }
    )
    val portsEOI = transpose(inC.zip(inC.map { i => outputIdRangesC.map { o => o.contains(i.e.bits.sink) } }).map {
      case (i, r) => TLXbar.fanout(i.e, r)
    })

    val beatsCI = inC.zip(edgesInC).map { case (i, e) => e.numBeats1(i.c.bits) }
    val beatsEI = inC.zip(edgesInC).map { case (i, e) => e.numBeats1(i.e.bits) }

    outC.indices.map { o =>
      TLArbiter(policy)(outC(o).c, filter(beatsCI.zip(portsCOI(o)), connectCOI(o)): _*)
      TLArbiter(policy)(outC(o).e, filter(beatsEI.zip(portsEOI(o)), connectEOI(o)): _*)
    }

    val beatsBO = outC.zip(edgesOutC).map { case (o, e) => e.numBeats1(o.b.bits) }
    inC.indices.map { i =>
      TLArbiter(policy)(inC(i).b, filter(beatsBO.zip(portsBIO(i)), connectBIO(i)): _*)
    }
  }

}

package uncore.tilelink

import chisel3._
import chisel3.util.{log2Ceil, DecoupledIO, Mux1H}
import diplomacy.{LazyModule, LazyModuleImp}
import logger.LazyLogging
import org.chipsalliance.utils.addressing.{AddressDecoder, AddressSet, IdRange, RegionType}
import tilelink.{
  TLBundle,
  TLBundleParameters,
  TLChannel,
  TLClientPortParameters,
  TLDecoupledNexusNode,
  TLManagerPortParameters
}

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
  def mapInputIds(ports:  Seq[TLClientPortParameters]):  Seq[IdRange] = assignRanges(ports.map(_.endSourceId))
  def mapOutputIds(ports: Seq[TLManagerPortParameters]): Seq[IdRange] = assignRanges(ports.map(_.endSinkId))
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
    for (i <- 0 until select.size) {
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
        clients = TLXbar.mapInputIds(seq).zip(seq).flatMap {
          case (range, port) =>
            port.clients.map { client =>
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
        managers = seq.flatMap { port =>
          require(
            port.channelBeatBytes == seq.head.channelBeatBytes,
            s"""Xbar ($name with parent $parent) data widths don't match: 
               |${port.managers.map(_.name)} has ${port.channelBeatBytes}B
               |${seq.head.managers.map(_.name)} has ${seq.head.channelBeatBytes}B""".stripMargin
          )
          val fifoDomainMapper = fifoDomainFactory()
          port.managers.map { manager =>
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

  lazy val module = new LazyModuleImp(this) {
    logger.debug(s"Generating TLXBar $pathName with ${node.in.size} Clients and ${node.out.size} Managers.")
    val (io_in, edgesIn) = node.in.unzip
    val (io_out, edgesOut) = node.out.unzip

    // Not every master need connect to every slave on every channel; determine which connections are necessary
    val reachableIO = edgesIn.map { cp =>
      edgesOut.map { mp =>
        cp.clientPortParameters.clients.exists { c =>
          mp.managerPortParameters.managers.exists { m =>
            c.visibility.exists { ca =>
              m.address.exists { ma =>
                ca.overlaps(ma)
              }
            }
          }
        }
      }.toVector
    }.toVector
    val probeIO = (edgesIn
      .zip(reachableIO))
      .map {
        case (cp, reachableO) =>
          (edgesOut
            .zip(reachableO))
            .map {
              case (mp, reachable) =>
                reachable &&
                  cp.clientPortParameters.anySupportProbe &&
                  mp.managerPortParameters.managers.exists(_.regionType >= RegionType.TRACKED)
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
                reachable && cp.clientPortParameters.anySupportProbe && mp.managerPortParameters.anySupportAcquiredB
            }
            .toVector
      }
      .toVector

    val connectAIO = reachableIO
    val connectBIO = probeIO
    val connectCIO = releaseIO
    val connectDIO = reachableIO
    val connectEIO = releaseIO

    def transpose[T](x: Seq[Seq[T]]) =
      if (x.isEmpty) Nil else Vector.tabulate(x(0).size) { i => Vector.tabulate(x.size) { j => x(j)(i) } }
    val connectAOI = transpose(connectAIO)
    val connectBOI = transpose(connectBIO)
    val connectCOI = transpose(connectCIO)
    val connectDOI = transpose(connectDIO)
    val connectEOI = transpose(connectEIO)

    // Grab the port ID mapping
    val inputIdRanges = TLXbar.mapInputIds(edgesIn.map(_.clientPortParameters))
    val outputIdRanges = TLXbar.mapOutputIds(edgesOut.map(_.managerPortParameters))

    // We need an intermediate size of bundle with the widest possible identifiers
    val wide_bundle = TLBundleParameters.union(io_in.map(_.bundleParameters) ++ io_out.map(_.bundleParameters))

    // Handle size = 1 gracefully (Chisel3 empty range is broken)
    def trim(id: UInt, size: Int) = if (size <= 1) 0.U else id(log2Ceil(size) - 1, 0)

    // Transform input bundle sources (sinks use global namespace on both sides)
    val in = Wire(Vec(io_in.size, TLBundle.decoupled(wide_bundle)))
    for (i <- in.indices) {
      val r = inputIdRanges(i)

      if (connectAIO(i).exists(x => x)) {
        in(i).a <> io_in(i).a
        in(i).a.bits.source := io_in(i).a.bits.source | r.start.U
      } else {
        in(i).a.valid := false.B
        io_in(i).a.ready := true.B
      }

      if (connectBIO(i).exists(x => x)) {
        io_in(i).b <> in(i).b
        io_in(i).b.bits.source := trim(in(i).b.bits.source, r.size)
      } else {
        in(i).b.ready := true.B
        io_in(i).b.valid := false.B
      }

      if (connectCIO(i).exists(x => x)) {
        in(i).c <> io_in(i).c
        in(i).c.bits.source := io_in(i).c.bits.source | r.start.U
      } else {
        in(i).c.valid := false.B
        io_in(i).c.ready := true.B
      }

      if (connectDIO(i).exists(x => x)) {
        io_in(i).d <> in(i).d
        io_in(i).d.bits.source := trim(in(i).d.bits.source, r.size)
      } else {
        in(i).d.ready := true.B
        io_in(i).d.valid := false.B
      }

      if (connectEIO(i).exists(x => x)) {
        in(i).e <> io_in(i).e
      } else {
        in(i).e.valid := false.B
        io_in(i).e.ready := true.B
      }
    }

    // Transform output bundle sinks (sources use global namespace on both sides)
    val out = Wire(Vec(io_out.size, TLBundle.decoupled(wide_bundle)))
    for (o <- out.indices) {
      val r = outputIdRanges(o)

      if (connectAOI(o).exists(x => x)) {
        io_out(o).a <> out(o).a
      } else {
        out(o).a.ready := true.B
        io_out(o).a.valid := false.B
      }

      if (connectBOI(o).exists(x => x)) {
        out(o).b <> io_out(o).b
      } else {
        out(o).b.valid := false.B
        io_out(o).b.ready := true.B
      }

      if (connectCOI(o).exists(x => x)) {
        io_out(o).c <> out(o).c
      } else {
        out(o).c.ready := true.B
        io_out(o).c.valid := false.B
      }

      if (connectDOI(o).exists(x => x)) {
        out(o).d <> io_out(o).d
        out(o).d.bits.sink := io_out(o).d.bits.sink | r.start.U
      } else {
        out(o).d.valid := false.B
        io_out(o).d.ready := true.B
      }

      if (connectEOI(o).exists(x => x)) {
        io_out(o).e <> out(o).e
        io_out(o).e.bits.sink := trim(out(o).e.bits.sink, r.size)
      } else {
        out(o).e.ready := true.B
        io_out(o).e.valid := false.B
      }
    }

    // Filter a list to only those elements selected
    def filter[T](data: Seq[T], mask: Seq[Boolean]) = (data.zip(mask)).filter(_._2).map(_._1)

    // Based on input=>output connectivity, create per-input minimal address decode circuits
    val requiredAC = (connectAIO ++ connectCIO).distinct
    val outputPortFns: Map[Vector[Boolean], Seq[UInt => Bool]] = requiredAC.map { connectO =>
      val port_addrs = edgesOut.map(_.managerPortParameters.managers.flatMap(_.address))
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
    if (false) {
      println(s"XBar ${name} mapping:")
      (edgesIn.zip(inputIdRanges)).zipWithIndex.foreach {
        case ((edge, id), i) =>
          println(s"\t$i assigned ${id} for ${edge.clientPortParameters.clients.map(_.name).mkString(", ")}")
      }
      println("")
    }

    val addressA = (in.zip(edgesIn)).map { case (i, e) => e.address(i.a.bits) }
    val addressC = (in.zip(edgesIn)).map { case (i, e) => e.address(i.c.bits) }

    def unique(x: Vector[Boolean]) = (x.count(x => x) <= 1).B
    val requestAIO = (connectAIO.zip(addressA)).map { case (c, i) => outputPortFns(c).map { o => unique(c) || o(i) } }
    val requestCIO = (connectCIO.zip(addressC)).map { case (c, i) => outputPortFns(c).map { o => unique(c) || o(i) } }
    val requestBOI = out.map { o => inputIdRanges.map { i => i.contains(o.b.bits.source) } }
    val requestDOI = out.map { o => inputIdRanges.map { i => i.contains(o.d.bits.source) } }
    val requestEIO = in.map { i => outputIdRanges.map { o => o.contains(i.e.bits.sink) } }

    val beatsAI = (in.zip(edgesIn)).map { case (i, e) => e.numBeats1(i.a.bits) }
    val beatsBO = (out.zip(edgesOut)).map { case (o, e) => e.numBeats1(o.b.bits) }
    val beatsCI = (in.zip(edgesIn)).map { case (i, e) => e.numBeats1(i.c.bits) }
    val beatsDO = (out.zip(edgesOut)).map { case (o, e) => e.numBeats1(o.d.bits) }
    val beatsEI = (in.zip(edgesIn)).map { case (i, e) => e.numBeats1(i.e.bits) }

    // Fanout the input sources to the output sinks
    val portsAOI = transpose(
      in.zip(requestAIO).map { case (i, r) => TLXbar.fanout(i.a, r) }
    )
    val portsBIO = transpose(
      out.zip(requestBOI).map { case (o, r) => TLXbar.fanout(o.b, r) }
    )
    val portsCOI = transpose(
      in.zip(requestCIO).map { case (i, r) => TLXbar.fanout(i.c, r) }
    )
    val portsDIO = transpose(
      out.zip(requestDOI).map { case (o, r) => TLXbar.fanout(o.d, r) }
    )
    val portsEOI = transpose(
      in.zip(requestEIO).map { case (i, r) => TLXbar.fanout(i.e, r) }
    )

    // Arbitrate amongst the sources
    for (o <- 0 until out.size) {
      TLArbiter(policy)(out(o).a, filter(beatsAI.zip(portsAOI(o)), connectAOI(o)): _*)
      TLArbiter(policy)(out(o).c, filter(beatsCI.zip(portsCOI(o)), connectCOI(o)): _*)
      TLArbiter(policy)(out(o).e, filter(beatsEI.zip(portsEOI(o)), connectEOI(o)): _*)
    }

    for (i <- 0 until in.size) {
      TLArbiter(policy)(in(i).b, filter(beatsBO.zip(portsBIO(i)), connectBIO(i)): _*)
      TLArbiter(policy)(in(i).d, filter(beatsDO.zip(portsDIO(i)), connectDIO(i)): _*)
    }
  }
}

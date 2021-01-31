package uncore.test

import diplomacy.{InModuleBody, LazyModule, LazyModuleImpLike, SimpleLazyModule, ValName}
import org.chipsalliance.utils.addressing.RegionType.{TRACKED, UNCACHED}
import org.chipsalliance.utils.addressing.{AddressSet, IdRange, TransferSizes}
import tilelink.{
  AccessAckD,
  AccessAckDataD,
  AcquireBlockA,
  GetA,
  GrantAckE,
  GrantDataD,
  ProbeAckDataC,
  ProbeBlockB,
  TLChannelBeatBytes,
  TLDecoupledMasterNodeHandle,
  TLDecoupledSlaveNodeHandle,
  TLMasterParameters,
  TLMasterPortParameters,
  TLSlaveParameters,
  TLSlavePortParameters
}
import uncore.tilelink.TLXbar
import utest._

object TLXbarTest extends TestSuite {

  class XbarTest(
    genClientNodes:  Seq[() => TLDecoupledMasterNodeHandle],
    genManagerNodes: Seq[() => TLDecoupledSlaveNodeHandle])
      extends SimpleLazyModule {
    val clientNodes = genClientNodes.map(_())
    val managerNodes = genManagerNodes.map(_())
    val xbar = LazyModule(new TLXbar())
    managerNodes.map(_ := xbar.node)
    clientNodes.map(xbar.node := _)
    InModuleBody {
      clientNodes.zipWithIndex.map { case (c, i) => c.makeIOs()(ValName(s"client_$i")) }
      managerNodes.zipWithIndex.map { case (c, i) => c.makeIOs()(ValName(s"manager_$i")) }
    }
  }

  val tests = Tests {
    //    test("uncached XBar should emit.") {
    //      val clientNode = () => TLDecoupledClientNode(Seq(TLClientPortParameters(Seq(
    //        TLClientParameters(
    //          nodePath = Seq.empty,
    //          resources = Seq.empty,
    //          setName = Some("client"),
    //          visibility = Seq(AddressSet.everything),
    //          unusedRegionTypes = Set(),
    //          executesOnly = false,
    //          requestFifo = false,
    //          supports = Set(AccessAckD(false)),
    //          emits = Set(GetA(transferSizes = TransferSizes(128))),
    //          neverReleasesData = false,
    //          sourceId = IdRange(0, 0),
    //        )
    //      ))))
    //      val managerNode = () => TLDecoupledManagerNode(Seq(TLManagerPortParameters(
    //        managers = Seq(
    //          TLManagerParameters(
    //            nodePath = Seq.empty,
    //            resources = Seq.empty,
    //            setName = Some("manager"),
    //            address = Seq(AddressSet(0x100, 0xff)),
    //            regionType = UNCACHED,
    //            executable = false,
    //            fifoDomain = None,
    //            supports = Set(GetA(transferSizes = TransferSizes(128))),
    //            emits = Set(AccessAckDataD(transferSizes = TransferSizes(128), mayDeny = false, mayCorrupt = false))
    //          )),
    //        channelBeatBytes = TLChannelBeatBytes(8, 0, 0, 8),
    //        1
    //      )))
    //
    //      print(chisel3.stage.ChiselStage.emitVerilog(LazyModule(new XbarTest(Seq(clientNode), Seq(managerNode))).module))
    //    }
    test("cached XBar should emit.") {
      val clientNode = () =>
        TLDecoupledClientNode(
          Seq(
            TLMasterPortParameters(
              Vector(
                TLClientParameters(
                  nodePath = Seq.empty,
                  resources = Seq.empty,
                  setName = Some("client"),
                  visibility = Seq(AddressSet.everything),
                  unusedRegionTypes = Set(),
                  executesOnly = false,
                  requestFifo = false,
                  emits = Set(
                    AcquireBlockA(TransferSizes(128), NtoB = true, NtoT = true, BtoT = false),
                    ProbeAckDataC(
                      TransferSizes(128),
                      TtoB = true,
                      TtoN = true,
                      BtoN = true,
                      TtoT = true,
                      BtoB = true,
                      NtoN = true,
                      mayCorrupt = false
                    ),
                    GrantAckE()
                  ),
                  supports = Set(
                    ProbeBlockB(TransferSizes(128), toN = false, toB = true, toT = false),
                    GrantDataD(
                      TransferSizes(128),
                      toT = true,
                      toB = true,
                      toN = true,
                      mayDeny = true,
                      mayCorrupt = true
                    )
                  ),
                  neverReleasesData = false,
                  sourceId = IdRange(0, 0)
                )
              )
            )
          )
        )
      val managerNode = () =>
        TLDecoupledManagerNode(
          Seq(
            TLManagerPortParameters(
              managers = Seq(
                TLSlaveParameters(
                  nodePath = Seq.empty,
                  resources = Seq.empty,
                  setName = Some("manager"),
                  address = Seq(AddressSet(0x100, 0xff)),
                  regionType = TRACKED,
                  executable = false,
                  fifoDomain = None,
                  supports = Set(
                    AcquireBlockA(TransferSizes(128), NtoB = true, NtoT = true, BtoT = false),
                    ProbeAckDataC(
                      TransferSizes(128),
                      TtoB = true,
                      TtoN = true,
                      BtoN = true,
                      TtoT = true,
                      BtoB = true,
                      NtoN = true,
                      mayCorrupt = false
                    ),
                    GrantAckE()
                  ),
                  emits = Set(
                    ProbeBlockB(TransferSizes(128), toN = false, toB = true, toT = false),
                    GrantDataD(
                      TransferSizes(128),
                      toT = true,
                      toB = true,
                      toN = true,
                      mayDeny = true,
                      mayCorrupt = true
                    )
                  )
                )
              ),
              channelBeatBytes = TLChannelBeatBytes(8, 8, 8, 8),
              1
            )
          )
        )
      print(chisel3.stage.ChiselStage.emitVerilog(LazyModule(new XbarTest(Seq(clientNode), Seq(managerNode))).module))
    }
  }
}

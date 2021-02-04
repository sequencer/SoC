package tilelinkTest

import org.chipsalliance.utils.addressing.{AddressSet, IdRange, RegionType, TransferSizes}
import tilelink._
import utest._

/** This test should focus on [[TLMasterParameters.supports]] and [[TLMasterParameters.emits]] function to make sure all
  * type parameter work well.
  */
object TLParametersSpec extends TestSuite {
  def tests: Tests = Tests {
    val tlMasterParameters: TLMasterParameters = TLMasterParameters(
      nodePath = Seq.empty,
      setName = None,
      sourceId = IdRange(1, 4),
      requestFifo = false,
      resources = Seq.empty,
      visibility = Seq(AddressSet(0x100, 0xff), AddressSet(0x300, 0xff)),
      supports = Set(
        AccessAckD(mayDeny = true)
      ),
      emits = Set(
        PutFullDataA(transferSizes = TransferSizes(2, 128), mayCorrupt = true),
        PutPartialDataA(TransferSizes(2, 64), mayCorrupt = true)
      )
    )
    val tlSlaveParameters: TLSlaveParameters = TLSlaveParameters(
      nodePath = Seq.empty,
      resources = Seq.empty,
      setName = None,
      address = Seq(AddressSet(0x100, 0xff)),
      regionType = RegionType.TRACKED,
      executable = true,
      fifoDomain = None,
      supports = Set(
        PutFullDataA(transferSizes = TransferSizes(2, 128), mayCorrupt = true),
        PutPartialDataA(TransferSizes(2, 64), mayCorrupt = true)
      ),
      emits = Set(
        AccessAckD(mayDeny = true)
      )
    )
    test("support emit query with type parameter should work") {
      assert(tlMasterParameters.supports[AccessAckD].head == AccessAckD(mayDeny = true))
      assert(tlMasterParameters.emits[PutFullDataA].size == 1)
      assert(tlMasterParameters.emits[HasTransferSizes].size == 2)
      assert(tlSlaveParameters.supports[ChannelAMessage].size == 2)
      assert(tlSlaveParameters.emits[HasData].isEmpty)
    }
    test("setName should work") {
      assert(tlMasterParameters.name == "disconnected")
      assert(tlSlaveParameters.name == "disconnected")
      assert(tlMasterParameters.copy(setName = Some("someName")).name == "someName")
      assert(tlSlaveParameters.copy(setName = Some("someName")).name == "someName")
    }
    test("maxTransferSize should work") {
      assert(tlMasterParameters.maxTransferSize[TLChannelA] == 128)
      assert(tlSlaveParameters.maxTransferSize[TLChannelA] == 128)
      assert(tlMasterParameters.maxTransferSize[TLChannelD] == 0)
    }
  }
}

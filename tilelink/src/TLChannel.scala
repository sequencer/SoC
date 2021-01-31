package tilelink

import chisel3._
import org.chipsalliance.utils.addressing.MaskGen

import scala.math.max

/** Parameter for [[TLChannel]],
  *
  * @param dataWidth is the width of [[TLDataChannel.data]]
  * @param sizeWidth is the width of [[TLDataChannel.size]]
  * @param addressWidth is the width of [[TLAddressChannel.address]]
  * @param sourceWidth is the width of [[TLSourceChannel.source]]
  * @param sinkWidth is the width of [[TLSinkChannel.sink]]
  */
case class TLChannelParameters(
  dataWidth:    Int = 0,
  sizeWidth:    Int = 0,
  addressWidth: Int = 0,
  sourceWidth:  Int = 0,
  sinkWidth:    Int = 0) {

  /** Size of bytes can be transferred on [[TLDataChannel]] for each beat of message. */
  def beatBytes: Int = dataWidth / 8

  /** width for [[TLMaskChannel.mask]]. */
  def maskWidth: Int = dataWidth / 8
}

object TLChannelParameters {

  /** function to union a sequence of [[TLChannelParameters]] by selecting max width of them. */
  private[tilelink] def union(channelParametersList: TLChannelParameters*): TLChannelParameters =
    channelParametersList.reduce { (l: TLChannelParameters, r: TLChannelParameters) =>
      TLChannelParameters(
        addressWidth = max(l.addressWidth, r.addressWidth),
        dataWidth = max(l.dataWidth, r.dataWidth),
        sourceWidth = max(l.sourceWidth, r.sourceWidth),
        sinkWidth = max(l.sinkWidth, r.sinkWidth),
        sizeWidth = max(l.sizeWidth, r.sizeWidth)
      )
    }

  /** function to union a sequence of optional [[TLChannelParameters]]. */
  private[tilelink] def union(channelParametersList: Option[TLChannelParameters]*): Option[TLChannelParameters] =
    channelParametersList.reduce { (l: Option[TLChannelParameters], r: Option[TLChannelParameters]) =>
      (l, r) match {
        case (Some(bp0), Some(bp1)) => Some(TLChannelParameters.union(bp0, bp1))
        case (Some(bp0), None)      => Some(bp0)
        case (None, Some(bp1))      => Some(bp1)
        case (None, None)           => None
      }
    }
}

/** TileLink Spec 1.8.1 Table 14
  *
  * Summary of TileLink routing fields
  *
  * | Channel | Destination | Sequence | Routed By | Provides  | For Use As |
  * | :---:   | :---:       | :---:    | :---:     | :---:     | :---:      |
  * | A       | slave       | request  | a_address | a_source  | d_source   |
  * | B       | master      | request  | b_source  | b_address | c_address  |
  * | C       | slave       | response | c_address | .         |            |
  * | C       | slave       | request  | c_address | c_source  |            |
  * | D       | master      | response | d_source  | .         | d_source   |
  * | D       | master      | request  | d_source  | d_sink    | e_sink     |
  * | E       | slave       | response | e_source  | .         |            |
  */
sealed trait TLChannel extends Bundle {
  val channelParameters: TLChannelParameters
}

sealed trait TLMasterToSlaveChannel extends TLChannel
sealed trait TLSlaveToMasterChannel extends TLChannel

/** Apply to ABCD channel. */
sealed trait TLOpcodeChannel extends TLChannel {
  val opcode: UInt = UInt(3.W)
  val param:  UInt = UInt(3.W)

  /** function to indicate a [[TLDataChannel]] contains data.
    * But this function is not exposed to user.
    * User should use [[TLEdge.hasData]] with static optimization
    */
  private[tilelink] def hasData: Bool
}

/** Apply to ABCD channel. */
sealed trait TLDataChannel extends TLChannel {
  val data:    UInt = UInt(channelParameters.dataWidth.W)
  val size:    UInt = UInt(channelParameters.sizeWidth.W)
  val corrupt: Bool = Bool()
}

/** Apply to AB channel. */
sealed trait TLMaskChannel extends TLChannel with TLAddressChannel with TLDataChannel {
  val mask: UInt = UInt(channelParameters.maskWidth.W)

  /** generate mask */
  def genMask: UInt = MaskGen(address, size, channelParameters.beatBytes)
}

/** only D channel can deny. */
sealed trait TLDeniedChannel extends TLChannel {
  val denied: Bool = Bool()
}

/** Apply to ABC channel. */
sealed trait TLAddressChannel extends TLChannel with TLDataChannel {
  val address: UInt = UInt(channelParameters.addressWidth.W)
}

/** Apply to ABCD channel. */
sealed trait TLSourceChannel extends TLChannel with TLDataChannel {
  val source: UInt = UInt(channelParameters.sourceWidth.W)
}

/** Apply to DE channel. */
sealed trait TLSinkChannel extends TLChannel {
  val sink: UInt = UInt(channelParameters.sinkWidth.W)
}

/** TileLink A Channel, refer to Spec 3.3 */
final class TLChannelA(val channelParameters: TLChannelParameters)
    extends TLChannel
    with TLOpcodeChannel
    // Request provides `a.source` for use as `d.source`
    with TLSourceChannel
    // Request routed by `a.address`
    with TLAddressChannel
    with TLMaskChannel
    with TLDataChannel
    with TLMasterToSlaveChannel {
  def hasData: Bool = !opcode(2)
}

/** TileLink B Channel, refer to Spec 3.4 */
final class TLChannelB(val channelParameters: TLChannelParameters)
    extends TLChannel
    with TLOpcodeChannel
    // Request routed by `b.source`
    with TLSourceChannel
    // Request provides `b.address` for use as `c.address`
    with TLAddressChannel
    with TLMaskChannel
    with TLDataChannel
    with TLSlaveToMasterChannel {
  def hasData: Bool = !opcode(2)
}

/** TileLink C Channel, refer to Spec 3.5 */
final class TLChannelC(val channelParameters: TLChannelParameters)
    extends TLChannel
    with TLOpcodeChannel
    // Request provides `c.source` for use as `d.source`
    with TLSourceChannel
    // Response routed by `c.address` provided by `a.address`
    // Request routed by `c.address`
    with TLAddressChannel
    with TLDataChannel
    with TLMasterToSlaveChannel {
  def hasData: Bool = opcode(0)
}

/** TileLink D Channel, refer to Spec 3.6 */
final class TLChannelD(val channelParameters: TLChannelParameters)
    extends TLChannel
    with TLOpcodeChannel
    // Response routed by `d.source` provided by `a.source`
    // Request routed by `d.source`
    with TLSourceChannel
    // Request provides `d.sink` for use as `e.sink`
    with TLSinkChannel
    with TLDataChannel
    with TLDeniedChannel
    with TLSlaveToMasterChannel {
  def hasData: Bool = opcode(0)
}

/** TileLink E Channel, refer to Spec 3.7 */
final class TLChannelE(val channelParameters: TLChannelParameters)
    extends TLChannel
    // Response routed by `e.sink`
    with TLSinkChannel
    with TLMasterToSlaveChannel

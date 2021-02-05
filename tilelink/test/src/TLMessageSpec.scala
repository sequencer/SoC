package tilelinkTest

import org.chipsalliance.utils.addressing.TransferSizes
import tilelink._
import utest._
object TLMessageSpec extends TestSuite {
  def tests: Tests = Tests {
    val getA:            GetA = GetA()
    val arithmeticDataA: ArithmeticDataA = ArithmeticDataA()
    val logicalDataA:    LogicalDataA = LogicalDataA()
    val accessAckDataD:  AccessAckDataD = AccessAckDataD()
    val intentA:         IntentA = IntentA()
    val acquireBlockA:   AcquireBlockA = AcquireBlockA()
    val grantD:          GrantD = GrantD()
    val probeAckC:       ProbeAckC = ProbeAckC()
    test("GetA") {
      // test exclusive rule
      test("can not support other") {
        assert(
          !getA.support(
            arithmeticDataA
          )
        )
      }
      test("can support itself") {
        assert(
          getA.support(
            getA
          )
        )
      }
      // test [[TransferSize]]
      test("can support smaller size") {
        assert(
          getA.support(
            getA
              .copy(transferSizes = TransferSizes(getA.transferSizes.min / 2, getA.transferSizes.max / 2))
          )
        )
      }
      test("can not support larger size") {
        assert(
          !getA.support(
            getA
              .copy(transferSizes = TransferSizes(getA.transferSizes.min * 2, getA.transferSizes.max * 2))
          )
        )
      }
    }
    test("ArithmeticDataA") {
      // test [[IsArithmetic]]
      test("none MIN can not support has MIN") {
        assert(
          !arithmeticDataA
            .copy(MIN = false)
            .support(
              arithmeticDataA
            )
        )
      }
      test("has MIN can not support none MIN") {
        assert(
          arithmeticDataA
            .support(
              arithmeticDataA
                .copy(MIN = false)
            )
        )
      }
      // test [[HasData]]
      test("none mayCorrupt can not support has mayCorrupt") {
        assert(
          !arithmeticDataA
            .copy(mayCorrupt = false)
            .support(
              arithmeticDataA
            )
        )
      }
      test("has mayCorrupt can not support none mayCorrupt") {
        assert(
          arithmeticDataA
            .support(
              arithmeticDataA
                .copy(mayCorrupt = false)
            )
        )
      }
    }
    test("LogicalDataA") {
      // test [[IsLogical]]
      test("none XOR can not support has XOR") {
        assert(
          !logicalDataA
            .copy(XOR = false)
            .support(
              logicalDataA
            )
        )
      }
      test("has XOR can not support none XOR") {
        assert(
          logicalDataA
            .support(
              logicalDataA
                .copy(XOR = false)
            )
        )
      }
    }
    test("AccessAckDataD") {
      // test [[HasDeny]]
      test("none XOR can not support has XOR") {
        assert(
          !accessAckDataD
            .copy(mayDeny = false)
            .support(
              accessAckDataD
            )
        )
      }
      test("has XOR can not support none XOR") {
        assert(
          accessAckDataD
            .support(
              accessAckDataD
                .copy(mayDeny = false)
            )
        )
      }
    }
    test("IntentA") {
      // test [[IsPrefetch]]
      test("none PrefetchRead can not support has PrefetchRead") {
        assert(
          !intentA
            .copy(PrefetchRead = false)
            .support(
              intentA
            )
        )
      }
      test("has PrefetchRead can not support none PrefetchRead") {
        assert(
          intentA
            .support(
              intentA
                .copy(PrefetchRead = false)
            )
        )
      }
    }
    // @todo not sure if this is a good behavior for [[HasGrow]]
    test("AcquireBlockA") {
      // test [[HasGrow]]
      test("none NtoB can not support has NtoB") {
        assert(
          !acquireBlockA
            .copy(NtoB = false)
            .support(
              acquireBlockA
            )
        )
      }
      test("has NtoB can not support none NtoB") {
        assert(
          acquireBlockA
            .support(
              acquireBlockA
                .copy(NtoB = false)
            )
        )
      }
    }
    // @todo how about always grant T here?
    test("GrantD") {
      // test [[HasCap]]
      test("none toT can not support has toT") {
        assert(
          !grantD
            .copy(toT = false)
            .support(
              grantD
            )
        )
      }
      test("has toT can not support none toT") {
        assert(
          grantD
            .support(
              grantD
                .copy(toT = false)
            )
        )
      }
    }
    test("ProbeAckC") {
      // test [[HasPruneOrReport]]
      test("none TtoB can not support has TtoB") {
        assert(
          !probeAckC
            .copy(TtoB = false)
            .support(
              probeAckC
            )
        )
      }
      test("has TtoB can not support none TtoB") {
        assert(
          probeAckC
            .support(
              probeAckC
                .copy(TtoB = false)
            )
        )
      }
    }
  }
}

package diplomatictester

import freechips.rocketchip.tilelink._
import chisel3._
import chisel3.experimental.BundleLiterals._

object TLEdgeLit {

  implicit class AddTileLinkLit[T <: TLEdge](edge: T) {
    /**
      * A [[Get]] message is a request made by an agent that would like to access a particular block
      * of data in order to read it.
      *
      * @param size    indicates the total amount of data the requesting agent wishes to read, in terms of log2(bytes).
      *                a size represents the size of the resulting [[AccessAckData]] response message, not this particular [[Get]] message.
      *                In TL-UL, a size cannot be larger than the width of the physical data bus.
      * @param source  is the transaction identifier of the Master Agent issuing this request. It will be copied by
      *                the Slave Agent to ensure the response is routed correctly
      * @param address must be aligned to size
      * @param mask    selects the byte lanes to read (Section 4.6). a size, a address and a mask are required
      *                to correspond with one another. Get must have a contiguous mask that is naturally aligned.
      * @note Supported protocol: TL-UL, TL-UH, TL-C
      * @todo add constraints check.
      **/
    def Get(size: Int,
            source: Int,
            address: Int,
            mask: Int): TLBundleA = (new TLBundleA(edge.bundle)).Lit(
      _.opcode -> 4.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> false.B
    )

    /**
      * A [[PutFullData]] message is a request made by an agent that would like to access a particular
      * block of data in order to write it.
      *
      * @param size    indicates the total amount of data the requesting agent wishes to write, in terms of
      *                log2(bytes). In this case, `size` represents the size of this request message
      * @param source  is the ID of the Master Agent that is the target of this request. It is used to route the
      *                request.
      * @param address must be aligned to `size`. The entire contents of `address` to `address+2**size-1` will be written.
      * @param mask    provides the byte select lanes, in this case indicating which bytes to write.
      *                One bit of `mask` corresponds to one byte of data written. `size`, `address` and `mask` are required to
      *                correspond with one another. [[PutFullData]] must have a contiguous mask,
      *                and if `size` is greater than or equal the width of the physical data bus then all `mask` must be HIGH.
      * @param corrupt Whether this beat of data is corrupt.
      * @param data    is the actual data payload to be written. `corrupt` being HIGH indicates the data in this beat is corrupt.
      * @note Supported protocol: TL-UL, TL-UH, TL-C
      * @todo add constraints check.
      **/
    def PutFullData(size: Int,
                    source: Int,
                    address: Int,
                    mask: Int,
                    corrupt: Boolean,
                    data: BigInt): TLBundleA = (new TLBundleA(edge.bundle)).Lit(
      _.opcode -> 0.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> corrupt.B,
      _.data -> data.U
    )

    /**
      * A [[PutPartialData]] message is a request made by an agent that would like to access a particular
      * block of data in order to write it. [[PutPartialData]] can be used to write arbitrary-aligned data
      * at a byte granularity.
      *
      * @param size    indicates the range of data the requesting agent will possibly write, in terms of log2(bytes).
      *                b size also represents the size of this request message’s data.
      * @param source  is the ID of the master interface that is the target of this request. It is used to route the request.
      * @param address must be aligned to `size`. Some subset of the contents of `address` to `address+2**size-1` will be written.
      * @param mask    provides the byte select lanes, in this case indicating which bytes to write.
      *                One bit of b mask corresponds to one byte of data written. `size`, `address` and `mask` are required to
      *                correspond with one another, but [[PutPartialData]] may write less data than `size`, depending on the contents of `mask`.
      *                Any set bits of b mask must be contained within an aligned region of `size`.
      * @param corrupt whether this beat of data is corrupt.
      * @param data    a is the actual data payload to be written. `data` in a byte that is unmasked is ignored and
      *                can take any value. `corrupt` being HIGH indicates that masked data in this beat is corrupt.
      * @note Supported protocol: TL-UL, TL-UH, TL-C
      * @todo add constraints check.
      **/
    def PutPartialData(size: Int,
                       source: Int,
                       address: Int,
                       mask: Int,
                       corrupt: Boolean,
                       data: BigInt): TLBundleB = new TLBundleB(edge.bundle).Lit(
      _.opcode -> 1.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> corrupt.B,
      _.data -> data.U
    )

    /**
      * [[AccessAck]] serves as a data-less acknowledgement message to the original requesting agent.
      *
      * @param size   contains the size of the data that was accessed, though this particular message contains
      *               no data itself. In a request/response message pair, `size` and `size` of a channel must always correspond.
      *               In TL-UL, d size cannot be larger than the width of the physical data bus.
      * @param source is the ID the of the agent issuing this response message.
      * @param denied indicates that the slave did not process the memory access.
      * @note Supported protocol: TL-UL, TL-UH, TL-C
      * @todo add constraints check.
      **/
    def AccessAck(size: Int,
                  source: Int,
                  denied: Boolean): TLBundleD = new TLBundleD(edge.bundle).Lit(
      _.opcode -> 0.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.denied -> denied.B,
      _.corrupt -> false.B
    )

    /**
      * [[AccessAckData]] serves as an acknowledgement message including data to the original requesting
      * agent.
      *
      * @param size    contains the size of the data that was accessed, which corresponds to the size of the
      *                data being included in this particular message. In a request/response message pair, `size` and
      *                `size` of a channel must always correspond. In TL-UL, `size` cannot be larger than the width of the physical
      *                data bus.
      * @param source  was saved from a source in the request and is now used to route this response to the
      *                correct destination
      * @param corrupt being HIGH indicates that masked data in this beat is corrupt.
      * @param denied  indicates that the slave did not process the memory access. If denied is HIGH then `corrupt` must also be high.
      * @param data    a contains the data that was accessed by the operation.
      * @note Supported protocol: TL-UL, TL-UH, TL-C
      * @todo add constraints check.
      **/
    def AccessAckData(size: Int,
                      source: Int,
                      denied: Boolean,
                      corrupt: Boolean,
                      data: BigInt): TLBundleD = new TLBundleD(edge.bundle).Lit(
      _.opcode -> 1.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.denied -> denied.B,
      _.corrupt -> corrupt.B,
      _.data -> data.U
    )

    /**
      * An [[ArithmeticData]] message is a request made by an agent that would like to access a particular
      * block of data in order to read-modify-write it by applying an arithmetic operation.
      *
      * @note Supported protocol: TL-UH, TL-C
      * @todo add constraints check.
      **/

    def ArithmeticData: TLBundleA = new TLBundleA(edge.bundle)

    /**
      * A [[LogicalData]] message is a request made by an agent that would like to access a particular
      * block of data in order to read-modify-write it by applying a bitwise logical operation.
      *
      * @note Supported protocol: TL-UH, TL-C
      * @todo add constraints check.
      **/
    def LogicalData: TLBundleA = new TLBundleA(edge.bundle)

    /**
      * A [[Intent]] message is a request made by an agent that would like to signal its future intention to
      * access a particular block of data.
      *
      * @note Supported protocol: TL-UH, TL-C
      * @todo add constraints check.
      **/
    def Intent: TLBundleA = new TLBundleA(edge.bundle)

    /**
      * [[HintAck]] serves as an acknowledgement message for a Hint operation.
      *
      * @note Supported protocol: TL-UH, TL-C
      * @todo add constraints check.
      **/
    def HintAck: TLBundleB = new TLBundleB(edge.bundle)

    /**
      * An [[AcquireBlock]] message is a request message type used by a Master Agent with a cache to
      * obtain a copy of a block of data that it plans to cache locally. Master Agents can also use this
      * message type to upgrade the permissions they have on a block already in their possession (i.e.,
      * to gain write permissions on a read-only copy).
      *
      * @note Supported protocol: TL-C
      * @todo add constraints check.
      **/
    def AcquireBlock: TLBundleA = new TLBundleA(edge.bundle)

    /**
      * An [[AcquirePerm]] message is a request message type used by a Master Agent with a cache
      * to upgrade permissions on a block without supplying a copy of the data contained in the block.
      * [[AcquirePerm]] must only be used in situations where no copy of the data is required to complete
      * the initiating operation. The primary example is the case where the block is being acquired in
      * order to be entirely overwritten.
      *
      * @note Supported protocol: TL-C
      * @todo add constraints check.
      **/
    def AcquirePerm: TLBundleA = new TLBundleA(edge.bundle)

    /**
      * A [[ProbeBlock]] message is a request message used by a Slave Agent to query or modify the
      * permissions of a cached copy of a data block stored by a particular Master Agent. A Slave Agent
      * may revoke a Master Agent’s permissions on a cache block either in response to an Acquire from
      * another master, or of its own volition. Table 8.6 shows all the fields of Channel B for this message
      * type.
      *
      * @note Supported protocol: TL-C
      * @todo add constraints check.
      **/
    def ProbeBlock: TLBundleB = new TLBundleB(edge.bundle)

    /**
      * A [[ProbePerm]] message is a request message used by a Slave Agent to query or modify the
      * permissions of a cached copy of a data block stored by a particular Master Agent. A Slave Agent
      * may revoke a Master Agent’s permissions on a cache block either in response to an Acquire from
      * another master, or of its own volition. [[ProbePerm]] must only be used in situations where no copy
      * of the data is required to complete the initiating operation. The primary example is the case where
      * the block is being acquired in order to be entirely overwritten.
      *
      * @note Supported protocol: TL-C
      * @todo add constraints check.
      **/
    def ProbePerm: TLBundleB = new TLBundleB(edge.bundle)

    /**
      * A [[ProbeAck]] message is a response message used by a Master Agent to acknowledge the receipt
      * of a Probe.
      *
      * @note Supported protocol: TL-C
      * @todo add constraints check.
      **/
    def ProbeAck: TLBundleC = new TLBundleC(edge.bundle)

    /**
      * A [[ProbeAckData]] message is a response message used by a Master Agent to acknowledge the
      * receipt of a Probe and write back dirty data that the requesting Slave Agent required.
      *
      * @note Supported protocol: TL-C
      * @todo add constraints check.
      **/
    def ProbeAckData: TLBundleC = new TLBundleC(edge.bundle)

    /**
      * A [[Grant]] message is both a response and a request message used by a Slave Agent to acknowledge
      * the receipt of a Acquire and provide permissions to access the cache block to the original
      * requesting Master Agent.
      *
      * @note Supported protocol: TL-C
      * @todo add constraints check.
      **/
    def Grant: TLBundleD = new TLBundleD(edge.bundle)

    /**
      * A [[GrantData]] message is a both a response and a request message used by a Slave Agent to
      * provide an acknowledgement along with a copy of the data block to the original requesting Master
      * Agent.
      *
      * @note Supported protocol: TL-C
      * @todo add constraints check.
      **/
    def GrantData: TLBundleD = new TLBundleD(edge.bundle)

    /**
      * The [[GrantAck]] response message is used by the Master Agent to provide a final acknowledgment
      * of transaction completion, and is in turn used to ensure global serialization of operations by the
      * Slave Agent.
      *
      * @note Supported protocol: TL-C
      * @todo add constraints check.
      **/
    def GrantAck: TLBundleE = new TLBundleE(edge.bundle)

    /**
      * A [[Release]] message is a request message used by a Master Agent to voluntarily downgrade its
      * permissions on a cached data block.
      *
      * @note Supported protocol: TL-C
      * @todo add constraints check.
      **/
    def Release: TLBundleC = new TLBundleC(edge.bundle)

    /**
      * A [[ReleaseData]] message is a request message used by a Master Agent to voluntarily downgrade
      * its permissions on a cached data block. and write back dirty data to the managing Slave Agent.
      *
      * @note Supported protocol: TL-C
      * @todo add constraints check.
      **/
    def ReleaseData: TLBundleC = new TLBundleC(edge.bundle)

    /**
      * A [[ReleaseAck]] message is a response message used by a Slave Agent to acknowledge the receipt
      * of a Release[Data], and is in turn used to ensure global serialization of operations by the Slave
      * Agent.
      *
      * @note Supported protocol: TL-C
      * @todo add constraints check.
      **/
    def ReleaseAck: TLBundleD = new TLBundleD(edge.bundle)
  }

}
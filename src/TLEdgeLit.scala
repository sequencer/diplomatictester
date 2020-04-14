package diplomatictester

import Chisel.DecoupledIO
import freechips.rocketchip.tilelink._
import chisel3._
import chisel3.experimental.BundleLiterals._

object TLEdgeLit {

  /** A simple explanation to TileLink:
    *
    * TileLink has 5 channels:
    * a master -> slave
    * Transmits a request that an operation be performed on a specified address range, accessing or caching the data.
    * implemented in [[AddTLOutLit]]
    * b master <- slave (TL-C only)
    * Transmits a request that an operation be performed at an address cached by a master agent, accessing or writing back that cached data.
    * implemented in [[AddTLInLit]]
    * c master -> slave (TL-C only)
    * Transmits a data or acknowledgment message in response to a request.
    * implemented in [[AddTLOutLit]]
    * d master <- slave
    * Transmits a data response or acknowledgement message to the original requestor.
    * implemented in [[AddTLInLit]]
    * e master -> slave (TL-C only)
    * Transmits a nal acknowledgment of a cache block transfer from the original requestor, used for serialization.
    * implemented in [[AddTLOutLit]]
    *
    *
    * architecture constraints:
    *   1. acyclic
    *   2. prioritization e > d > c > b > a
    *
    * */
  implicit class AddTLOutLit[T <: TLEdgeOut](edge: T) {
    /* TL-UL */
    /**
      * A [[Get]] message is a request made by an agent that would like to access a particular block
      * of data in order to read it.
      *
      * @param size    indicates the total amount of data the requesting agent wishes to read, in terms of log2(bytes).
      *                a size represents the size of the resulting [[AddTLInLit.AccessAckData]] response message, not this particular [[Get]] message.
      *                In TL-UL, a size cannot be larger than the width of the physical data bus.
      * @param source  is the transaction identifier of the Master Agent issuing this request. It will be copied by
      *                the Slave Agent to ensure the response is routed correctly
      * @param address must be aligned to size
      * @param mask    selects the byte lanes to read (Section 4.6). a size, a address and a mask are required
      *                to correspond with one another. Get must have a contiguous mask that is naturally aligned.
      * @note Supported protocol: TL-UL, TL-UH, TL-C
      **/
    def Get(size: Int,
            source: Int,
            address: Int,
            mask: Int): TLBundleA = new TLBundleA(edge.bundle).Lit(
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
      * @param corrupt being HIGH indicates that masked data in this beat is corrupt.
      * @param data    is the actual data payload to be written. `corrupt` being HIGH indicates the data in this beat is corrupt.
      * @note Supported protocol: TL-UL, TL-UH, TL-C
      **/
    def PutFullData(size: Int,
                    source: Int,
                    address: Int,
                    mask: Int,
                    corrupt: Boolean,
                    data: BigInt): TLBundleA = new TLBundleA(edge.bundle).Lit(
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
      * @param corrupt being HIGH indicates that masked data in this beat is corrupt.
      * @param data    a is the actual data payload to be written. `data` in a byte that is unmasked is ignored and
      *                can take any value. `corrupt` being HIGH indicates that masked data in this beat is corrupt.
      * @note Supported protocol: TL-UL, TL-UH, TL-C
      **/
    def PutPartialData(size: Int,
                       source: Int,
                       address: Int,
                       mask: Int,
                       corrupt: Boolean,
                       data: BigInt): TLBundleA = new TLBundleA(edge.bundle).Lit(
      _.opcode -> 1.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> corrupt.B,
      _.data -> data.U
    )

    /* TL-UH */
    /**
      * An [[ArithmeticData]] message is a request made by an agent that would like to access a particular
      * block of data in order to read-modify-write it by applying an arithmetic operation.
      *
      * @param param   specifies the specific atomic operation to perform. It consists of [[ArithmeticDataParam]],
      *                representing signed and unsigned integer maximum and minimum, as well as integer addition.
      * @param size    is the arithmetic operand size and reflects both the size of this request’s data as well as
      *                the [[AddTLInLit.AccessAckData]] response.
      * @param source  is the ID of the master interface that is the target of this request. It is used to route the
      *                request.
      * @param address must be aligned to `size`.
      * @param mask    provides the byte select lanes, in this case indicating which bytes to read-modify-write.
      *                One bit of `mask` corresponds to one byte of data used in the atomic operation.
      *                `size`, `address` and `mask` are required to correspond with one another (i.e., the `mask`
      *                is also naturally aligned and fully set HIGH contiguously within that alignment).
      * @param corrupt being HIGH indicates that masked data in this beat is corrupt.
      * @param data    contains one of the arithmetic operands (the other is found at the target address). Any
      *                byte of a data that is not masked by a mask is ignored and can take any value.
      * @note Supported protocol: TL-UH, TL-C
      **/
    def ArithmeticData(param: ArithmeticDataParam,
                       size: Int,
                       source: Int,
                       address: Int,
                       mask: Int,
                       corrupt: Boolean,
                       data: BigInt): TLBundleA = new TLBundleA(edge.bundle).Lit(
      _.opcode -> 2.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> corrupt.B,
      _.data -> data.U
    )

    /**
      * A [[LogicalData]] message is a request made by an agent that would like to access a particular
      * block of data in order to read-modify-write it by applying a bitwise logical operation.
      *
      * @param param   specifies the specific atomic bitwise logical operation to perform. It consists of [[LogicDataParam]],
      *                representing bitwise logical xor, or, and, as well as a simple swap of the operands.
      * @param size    is the operand size, in terms of log2(bytes). It reflects both the size of the this request’s
      *                data as well as the size of the [[AddTLInLit.AccessAckData]] response.
      * @param source  is the transaction identifier of the Master Agent issuing this request. It will be copied by
      *                the Slave Agent to ensure the response is routed correctly.
      * @param address must be aligned to `size`.
      * @param mask    selects the byte lanes to read-modify-write (Section 4.6). One HIGH bit of a mask corresponds to
      *                one byte of data used in the atomic operation. a size, a address and a mask are required to correspond
      *                with one another. The HIGH bits of a mask must also be naturally aligned and contiguous within
      *                that alignment.
      * @param corrupt being HIGH indicates that masked data in this beat is corrupt.
      * @param data    contains one of the logical operands (the other is found at the target address). Any byte of
      *                a data that is not masked by a mask is ignored and can take any value.
      * @note Supported protocol: TL-UH, TL-C
      **/
    def LogicalData(param: LogicDataParam,
                    size: Int,
                    source: Int,
                    address: Int,
                    mask: Int,
                    corrupt: Boolean,
                    data: BigInt): TLBundleA = new TLBundleA(edge.bundle).Lit(
      _.opcode -> 3.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> corrupt.B,
      _.data -> data.U
    )

    /**
      * A [[Intent]] message is a request made by an agent that would like to signal its future intention to
      * access a particular block of data.
      *
      * @param param   species the specific intention being conveyed by this Hint operation. Note that its intended
      *                effect applies to the slave interface and possibly agents further out in the hierarchy.
      *                It consists of [[IntentParam]], representing prefetch-data-with-intent-to-read and
      *                prefetch-data-with-intent-to-write.
      * @param size    is the size of the memory to which this intention applies
      * @param source  is the transaction identifier of the Master Agent issuing this request. It will be copied by the
      *                Slave Agent to ensure the response is routed correctly
      * @param address must be aligned to `size`.
      * @param mask    indicates the bytes to which the intention applies. `size`, `address` and
      *                `mask` are required to correspond with one another.
      * @note Supported protocol: TL-UH, TL-C
      **/
    def Intent(param: IntentParam,
               size: Int,
               source: Int,
               address: Int,
               mask: Int): TLBundleA = new TLBundleA(edge.bundle).Lit(
      _.opcode -> 5.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> false.B,
    )

    /* TL-C */
    /**
      * An [[AcquireBlock]] message is a request message type used by a Master Agent with a cache to
      * obtain a copy of a block of data that it plans to cache locally. Master Agents can also use this
      * message type to upgrade the permissions they have on a block already in their possession (i.e.,
      * to gain write permissions on a read-only copy).
      *
      * @param param   indicates the specific type of permissions change the Master Agent intends to occur.
      *                Possible transitions are selected from the [[Grow]] category.
      * @param size    indicates the total amount of data the requesting Master Agent wishes to cache, in terms of
      *                `log2(bytes)`.
      * @param source  is the ID of the Master Agent issuing this request. It will be used by the responding Slave
      *                Agent to ensure the response is routed correctly.
      * @param address must be aligned to `size`.
      * @param mask    provides the byte select lanes, in this case indicating which bytes to read.
      *                `size`, `address` and `mask` are required to correspond with one another. Acquires must
      *                have a contiguous mask that is naturally aligned.
      * @note Supported protocol: TL-C
      **/
    def AcquireBlock(param: Permission,
                     size: Int,
                     source: Int,
                     address: Int,
                     mask: Int,
                    ): TLBundleA = new TLBundleA(edge.bundle).Lit(
      _.opcode -> 6.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> false.B,
    )

    /**
      * An [[AcquirePerm]] message is a request message type used by a Master Agent with a cache
      * to upgrade permissions on a block without supplying a copy of the data contained in the block.
      * [[AcquirePerm]] must only be used in situations where no copy of the data is required to complete
      * the initiating operation. The primary example is the case where the block is being acquired in
      * order to be entirely overwritten.
      *
      * @param param   indicates the speci c type of permissions change the Master Agent intends to occur.
      *                Possible transitions are selected from the [[Grow]] category.
      * @param size    indicates the total amount of data the requesting Master Agent wishes to have permission to
      *                cache, in terms of log2(bytes). As in a [[Get]] message, an [[AcquirePerm]] message does not contain data
      *                itself. Additionally, no data will ever be returned to the requestor in response to this message.
      * @param source  is the ID of the Master Agent issuing this request. It will be used by the responding Slave
      *                Agent to ensure the response is routed correctly.
      * @param address must be aligned to `size`.
      * @param mask    provides the byte select lanes, in this case indicating which bytes to read.
      *                `size`, `address` and `mask` are required to correspond with one another.
      *                [[AcquirePerm]] must have a contiguous mask that is naturally aligned.
      * @note Supported protocol: TL-C
      **/
    def AcquirePerm(param: Permission,
                    size: Int,
                    source: Int,
                    address: Int,
                    mask: Int,
                   ): TLBundleA = new TLBundleA(edge.bundle).Lit(
      _.opcode -> 7.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> false.B,
    )

    /**
      * A [[ProbeAck]] message is a response message used by a Master Agent to acknowledge the receipt
      * of a Probe.
      *
      * @param param   indicates the specific type of permissions change that occurred in the Master Agent as a
      *                result of the Probe. Possible transitions are selected from the [[Shrink]] or [[Report]] category.
      *                The former indicates that permissions were decreased whereas the latter reports what they were and
      *                continue to be.
      * @param size    indicates the total amount of data that was probed, in terms of `log2(bytes)`. This message
      *                itself does not carry data.
      * @param source  is the ID of the Master Agent that is the source of this response.
      * @param address is used to route the response to the original requestor. It must be aligned to `size`.
      * @note Supported protocol: TL-C
      **/
    def ProbeAck(param: Permission,
                 size: Int,
                 source: Int,
                 address: Int): TLBundleC = new TLBundleC(edge.bundle).Lit(
      _.opcode -> 4.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.corrupt -> false.B,
    )

    /**
      * A [[ProbeAckData]] message is a response message used by a Master Agent to acknowledge the
      * receipt of a Probe and write back dirty data that the requesting Slave Agent required.
      *
      * @param param   indicates the specific type of permissions change that occurred in the Master Agent as a
      *                result of the Probe. Possible transitions are selected from the [[Shrink]] or [[Report]] category.
      *                The former indicates that permissions were decreased whereas the latter reports what they were and
      *                continue to be.
      * @param size    indicates the total amount of data that was probed, in terms of `log2(bytes)`, as well as the
      *                amount of data contained in this message.
      * @param source  is the ID of the Master Agent that is the source of this response, copied from `source`.
      * @param address is used to route the response to the original requestor.
      * @param corrupt `corrupt` being HIGH indicates that the data in this beat is corrupt.
      * @param data    contains the data accessed by the operation.
      * @note Supported protocol: TL-C
      **/
    def ProbeAckData(param: Permission,
                     size: Int,
                     source: Int,
                     address: Int,
                     corrupt: Boolean,
                     data: BigInt): TLBundleC = new TLBundleC(edge.bundle).Lit(
      _.opcode -> 5.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.corrupt -> corrupt.B,
      _.data -> data.B
    )

    /**
      * The [[GrantAck]] response message is used by the Master Agent to provide a final acknowledgment
      * of transaction completion, and is in turn used to ensure global serialization of operations by the
      * Slave Agent.
      *
      * @param sink should have been saved from the d_sink in the preceding [[AddTLInLit.Grant]] or
      *             [[AddTLInLit.GrantData]] message, and is now being re-used to route this response
      *             to the correct destination.
      * @note Supported protocol: TL-C
      **/
    def GrantAck(sink: Int): TLBundleE = new TLBundleE(edge.bundle).Lit(
      _.sink -> sink.U
    )

    /**
      * A [[Release]] message is a request message used by a Master Agent to voluntarily downgrade its
      * permissions on a cached data block.
      *
      * @param param   indicates the specific type of permissions change that the Master Agent is initiating.
      *                Possible transitions are selected from the [[Shrink]] or [[Report]] category,
      *                which indicates both what the permissions were and what they are becoming.
      * @param size    indicates the total amount of cached data whose permissions are being released, in terms of
      *                `log2(bytes)`. This message itself does not carry data.
      * @param source  is the ID of the Master Agent that is the source of this request. The ID does not have to be
      *                the same as the ID used to Acquire the block originally, though it must correspond to the same Master
      *                Agent.
      * @param address is used to route the response to the managing Slave Agent for that address. It must be
      *                aligned to `size`.
      * @note Supported protocol: TL-C
      **/
    def Release(param: Permission,
                size: Int,
                source: Int,
                address: Int): TLBundleC = new TLBundleC(edge.bundle).Lit(
      _.opcode -> 6.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.corrupt -> false.B
    )

    /**
      * A [[ReleaseData]] message is a request message used by a Master Agent to voluntarily downgrade
      * its permissions on a cached data block. and write back dirty data to the managing Slave Agent.
      *
      * @param param   indicates the specific type of permissions change that the Master Agent is initiating.
      *                Possible transitions are selected from the [[Shrink]] or [[Report]] category, which indicates both
      *                what the permissions were and what they are becoming.
      * @param size    indicates the total amount of cached data whose permissions are being released, in terms of
      *                `log2(bytes)`, as well as the amount of data contained in this message.
      * @param source  is the ID of the Master Agent that is the source of this response.
      * @param address is used to route the response to the original requestor. It must be aligned to `size`.
      * @param data    contains the dirty data being written back by the operation. `corrupt` being HIGH indicates
      *                that this beat of data is corrupt.
      * @note Supported protocol: TL-C
      **/
    def ReleaseData(param: Permission,
                    size: Int,
                    source: Int,
                    address: Int,
                    data: BigInt): TLBundleC = new TLBundleC(edge.bundle).Lit(
      _.opcode -> 7.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.corrupt -> false.B,
      _.data -> data.B
    )
  }

  implicit class AddTLInLit[T <: TLEdgeIn](edge: T) {
    /* TL-UL */
    /**
      * [[AccessAck]] serves as a data-less acknowledgement message to the original requesting agent.
      *
      * @param size   contains the size of the data that was accessed, though this particular message contains
      *               no data itself. In a request/response message pair, `size` and `size` of a channel must always correspond.
      *               In TL-UL, d size cannot be larger than the width of the physical data bus.
      * @param source is the ID the of the agent issuing this response message.
      * @param denied indicates that the slave did not process the memory access.
      * @note Supported protocol: TL-UL, TL-UH, TL-C
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

    /* TL-UH */
    /**
      * [[HintAck]] serves as an acknowledgement message for a Hint operation.
      *
      * @param size   contains the size of the data that was hinted about, though this particular message contains
      *               no data itself.
      * @param source was saved from `source` in the request and is now used to route this response to the
      *               correct destination。
      * @param denied indicates that the slave did not process the hint。
      * @note Supported protocol: TL-C
      **/
    def HintAck(size: Int,
                source: Int,
                denied: Boolean): TLBundleD = new TLBundleD(edge.bundle).Lit(
      _.opcode -> 2.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.denied -> denied.B,
      _.corrupt -> false.B,
    )

    /* TL-C */
    /**
      * A [[ProbeBlock]] message is a request message used by a Slave Agent to query or modify the
      * permissions of a cached copy of a data block stored by a particular Master Agent. A Slave Agent
      * may revoke a Master Agent’s permissions on a cache block either in response to an Acquire from
      * another master, or of its own volition. Table 8.6 shows all the fields of Channel B for this message
      * type.
      *
      * @param param   indicates the specific type of permissions change the Slave Agent intends to occur. Possible
      *                transitions are selected from the [[Cap]] category. Probing Master Agents to cap their
      *                permissions at a more permissive level than they currently have is allowed, and does not result in a
      *                permissions change.
      * @param size    indicates the total amount of data the requesting agent wishes to probe, in terms of `log2(bytes)`.
      *                If dirty data is written back in response to this probe, `size` represents the size of the
      *                resulting [[AddTLOutLit.ProbeAckData]] message, not this particular [[ProbeBlock]] message.
      * @param source  is the ID of the Master Agent that is the target of this request. It is used to route the
      *                request, e.g., to a particular cache.
      * @param address must be aligned to `size`.
      * @param mask    provides the byte select lanes, in this case indicating which bytes to probe. `size`, `address`
      *                and `mask` are required to correspond with one another. [[ProbeBlock]] messages must have a contiguous mask.
      * @note Supported protocol: TL-C
      **/
    def ProbeBlock(param: Permission,
                   size: Int,
                   source: Int,
                   address: Int,
                   mask: Int,
                  ): TLBundleB = new TLBundleB(edge.bundle).Lit(
      _.opcode -> 6.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> false.B,
    )

    /**
      * A [[ProbePerm]] message is a request message used by a Slave Agent to query or modify the
      * permissions of a cached copy of a data block stored by a particular Master Agent. A Slave Agent
      * may revoke a Master Agent’s permissions on a cache block either in response to an Acquire from
      * another master, or of its own volition. [[ProbePerm]] must only be used in situations where no copy
      * of the data is required to complete the initiating operation. The primary example is the case where
      * the block is being acquired in order to be entirely overwritten.
      *
      * @param param   indicates the specific type of permissions change the Slave Agent intends to occur. Possible
      *                transactions are selected from the [[Cap]] category. Probing Master Agents to cap their
      *                permissions at a more permissive level than they currently have is allowed, and does not result in a
      *                permissions change.
      * @param size    indicates the total amount of data the requesting agent wishes to probe, in terms of `log2(bytes)`.
      *                No data will ever be returned to the requestor in response to this message.
      * @param source  is the ID of the Master Agent that is the target of this request. It is used to route the
      *                request, e.g. to a particular cache.
      * @param address must be aligned to `size`.
      * @param mask    provides the byte select lanes, in this case indicating which bytes to probe. `size`, `address`
      *                and `mask` are required to correspond with one another. [[ProbePerm]] messages must have a contiguous mask.
      * @note Supported protocol: TL-C
      **/
    def ProbePerm(param: Permission,
                  size: Int,
                  source: Int,
                  address: Int,
                  mask: Int): TLBundleB = new TLBundleB(edge.bundle).Lit(
      _.opcode -> 7.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> false.B,
    )

    /**
      * A [[Grant]] message is both a response and a request message used by a Slave Agent to acknowledge
      * the receipt of a Acquire and provide permissions to access the cache block to the original
      * requesting Master Agent.
      *
      * @param param  indicates the specific type of accesses that the Slave Agent is granting permission to occur
      *               on the cached copy of the block in the Master Agent as a result of the Acquire request. Possible
      *               permission transitions are selected from the [[Cap]] category. Permissions are increased
      *               without specifying the original permissions. Permissions may exceed those requested by the `param`
      *               field of the original request.
      * @param size   contains the size of the data whose permissions are being transferred, though this particular
      *               message contains no data itself. Must be identical to the original `size`.
      * @param source The master source identifier receiving this response; copied from `source` of a channel. @todo upstream error
      * @param sink   is the identifier the of the agent issuing this message used to route its E response, whereas
      *               `source` should have been saved from `source` in the original Channel A request, and is now
      *               being re-used to route this response to the correct destination.
      * @param denied indicates that the slave did not process the permissions transfer. In this case, `param`
      *               should be ignored, meaning the coherence policy permissions of the block remain unchanged.
      * @note Supported protocol: TL-C
      **/
    def Grant(param: Permission,
              size: Int,
              source: Int,
              sink: Int,
              denied: Int): TLBundleD = new TLBundleD(edge.bundle).Lit(
      _.opcode -> 4.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> sink.U,
      _.denied -> denied.B,
      _.corrupt -> false.B,
    )

    /**
      * A [[GrantData]] message is a both a response and a request message used by a Slave Agent to
      * provide an acknowledgement along with a copy of the data block to the original requesting Master
      * Agent.
      *
      * @param param   indicates the specific type of accesses that the Slave Agent is granting permissions to occur
      *                on the cached copy of the block in the Master Agent as a result of the Acquire request. Possible
      *                permission transitions are selected from the [[Cap]] category. Permissions are increased
      *                without specifying the original permissions. Permissions may exceed those requested by the `param`
      *                field of the original request.
      * @param size    contains the size of the data whose permissions were transferred, though this particular
      *                message contains no data itself. It can be saved from the c_size in the preceding Release[Data]
      *                message.
      * @param source  The master source identifier receiving this response; copied from `source` of a channel.
      * @param sink    is the identifier the of the agent issuing this response message, whereas used to route its E
      *                response, whereas `source` should have been saved from `source` in the original Channel A
      *                request, and is now being re-used to route this response to the correct destination.
      * @param denied  indicates that the slave did not process the permissions transfer. In this case, `param`
      *                should be ignored, meaning the coherence policy permissions ofthe block remain unchanged.
      *                `corrupt` being HIGH indicates that the data in this beat is corrupt. If `denied` is HIGH then
      *                `corrupt` must also be high.
      * @param corrupt indicates whether this data beat is corrupt.
      * @param data    contains the data being transferred by the operation, which will be cached by the Master Agent.
      * @note Supported protocol: TL-C
      **/
    def GrantData(param: Permission,
                  size: Int,
                  source: Int,
                  sink: Int,
                  denied: Int,
                  corrupt: Boolean,
                  data: BigInt): TLBundleD = new TLBundleD(edge.bundle).Lit(
      _.opcode -> 4.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.sink -> sink.U,
      _.denied -> denied.B,
      _.corrupt -> corrupt.B,
      _.data -> data.U
    )

    /**
      * A [[ReleaseAck]] message is a response message used by a Slave Agent to acknowledge the receipt
      * of a Release[Data], and is in turn used to ensure global serialization of operations by the Slave
      * Agent.
      *
      * @param size   contains the size of the data whose permissions were transferred, though this particular
      *               message contains no data itself. It can be saved from the `size` in the preceding
      *               [[AddTLOutLit.Release]] or [[AddTLOutLit.ReleaseData]] message.
      * @param source should have been saved from the `source` of c channel in the preceding [[AddTLOutLit.Release]]
      *               or [[AddTLOutLit.ReleaseData]] message and is now being re-used to route this response to
      *               the correct destination. `sink` is ignored and does not need to be unique across the [[ReleaseAck]]s
      *               that are being sent to respond to any unanswered releases.
      * @note Supported protocol: TL-C
      **/
    def ReleaseAck(size: Int,
                   source: Int): TLBundleD = new TLBundleD(edge.bundle).Lit(
      _.opcode -> 6.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.denied -> false.B,
      _.corrupt -> false.B
    )

    /* TL-UL TL-UH on TL-C */
    /**
      * [[Get]] A Get message is a request made by an agent that would like to access a particular block of data in
      * order to read it.
      *
      * @param size    A Get message is a request made by an agent that would like to access a particular block of data in
      *                order to read it.
      * @param source  is the ID of the Master Agent that is the target of this request. It is used to route the
      *                request.
      * @param address must be aligned to `size`.
      * @param mask    provides the byte select lanes, in this case indicating which bytes to read. `size`, `address`
      *                and `mask` are required to correspond with one another. [[Get]] messages must have a contiguous mask.
      * @note Supported protocol: TL-C
      **/
    def Get(size: Int,
            source: Int,
            address: Int,
            mask: Int): TLBundleB = new TLBundleB(edge.bundle).Lit(
      _.opcode -> 4.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> false.B
    )

    /**
      * [[PutFullData]] A [[PutFullData]] message is a request by an agent that would like to access a particular block of data
      * in order to write it.
      *
      * @param size    indicates the total amount of data the requesting agent wishes to write, in terms of
      *                log2(bytes). In this case, size represents the size of this request message.
      * @param source  is the ID of the Master Agent that is the target of this request. It is used to route the
      *                request.
      * @param address must be aligned to `size`. The entire contents of `address` to `address + 2 * size - 1` will be written.
      * @param mask    provides the byte select lanes, in this case indicating which bytes to write.
      *                One bit of `mask` corresponds to one byte of data written. `size`, `address` and `mask`
      *                are required to correspond with one another. [[PutFullData]] must have a contiguous mask, and if
      *                `size` is greater than or equal the width of the physical data bus then all `mask` must be HIGH.
      * @param corrupt whether this beat of data is corrupt.
      * @param data    Data payload to be written.
      * @note Supported protocol: TL-C
      **/
    def PutFullData(size: Int,
                    source: Int,
                    address: Int,
                    mask: Int,
                    corrupt: Boolean,
                    data: BigInt): TLBundleB = new TLBundleB(edge.bundle).Lit(
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
      * [[PutPartialData]] is a request by an agent that would like to access a particular block of data in order to write it.
      * [[PutPartialData]] can be used to write arbitrary-aligned data at a byte granularity.
      *
      * @param size    indicates the range of data the requesting agent will possibly write, in terms of `log2(bytes)`.
      *                `size` also represents the size of this request message’s data.
      * @param source  is the ID of the master that is the target of this request. It is used to route the request.
      * @param address must be aligned to `size`. Some subset of the contents of `address` to `address + 2b_size - 1` will be written.
      * @param mask    provides the byte select lanes, in this case indicating which bytes to write.
      *                One bit of b_mask corresponds to one byte of data written. `size`, `address` and `mask`
      *                are required to correspond with one another, but [[PutPartialData]] may write less data than `size`,
      *                depending on the contents of `mask`. Any set bits of `mask` must be contained within an aligned
      *                region of `size`.
      * @param corrupt being HIGH indicates that masked data in this beat is corrupt.
      * @param data    is the actual data payload to be written. `data` in a byte that is unmasked is ignored and can
      *                take any value.
      * @note Supported protocol: TL-C
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
      * [[AccessAck]] provides a dataless acknowledgement to the original requesting agent.
      *
      * @param size    contains the size of the data that was accessed, though this particular message contains no
      *                data itself. The size and address fields must be aligned. `address` must match the `address` of b channel
      *                from the request that triggered this response. It is used to route this response back to the Tip.
      * @param source  is the ID the of the agent issuing this response message.
      * @param address the target address of the operation, in bytes.
      * @note Supported protocol: TL-C
      **/
    def AccessAck(size: Int,
                  source: Int,
                  address: Int): TLBundleC = new TLBundleC(edge.bundle).Lit(
      _.opcode -> 0.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.corrupt -> false.B
    )

    /**
      * [[AccessAckData]] provides an acknowledgement with data to the original requesting agent.
      *
      * @param size    contains the size of the data that was accessed, which corresponds to the size of the data
      *                associated with this particular message. The size and address fields must be aligned.
      * @param source  is the ID the of the agent issuing this response message.
      * @param address must match the `address` of b channel from the request that triggered this response. It is used to
      *                route this response back to the Tip.
      * @param corrupt being HIGH indicates that this beat of data is corrupt.
      * @param data    contains the data accessed by the operation. Data can be changed between beats of a
      *                [[AccessAckData]] that is a burst.
      * @note Supported protocol: TL-C
      **/
    def AccessAckData(size: Int,
                      source: Int,
                      address: Int,
                      corrupt: Boolean,
                      data: BigInt): TLBundleC = new TLBundleC(edge.bundle).Lit(
      _.opcode -> 1.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.corrupt -> corrupt.B,
      _.data -> data.U
    )


    /** An [[ArithmeticData]] message is a request made by an agent that would like to access a particular block of data
      * in order to read-modify-write it with an arithmetic operation.
      *
      * @param param   specifies the specific atomic operation to perform. It consists of [[ArithmeticDataParam]],
      *                representing signed and unsigned integer maximum and minimum, as well as integer addition.
      * @param size    is the arithmetic operand size and reflects both the size of this request’s data as well as the
      *                [[AccessAckData]] response.
      * @param source  is the ID of the master that is the target of this request. It is used to route the request.
      * @param address must be aligned to `size`.
      * @param mask    provides the byte select lanes, in this case indicating which bytes to read-modify-write.
      *                One bit of `mask` corresponds to one byte of data used in the atomic operation. `size`, `address`
      *                and `mask` are required to correspond with one another (i.e., the mask is also naturally aligned
      *                and fully set HIGH contiguously within that alignment).
      * @param corrupt `corrupt` being HIGH indicates that masked data in this beat is corrupt.
      * @param data    contains one of the operands (the other is found at the target address). `data` in a byte that
      *                is unmasked is ignored and can take any value.
      * @note Supported protocol: TL-C
      * */
    def ArithmeticData(param: ArithmeticDataParam,
                       size: Int,
                       source: Int,
                       address: Int,
                       mask: Int,
                       corrupt: Boolean,
                       data: BigInt): TLBundleB = new TLBundleB(edge.bundle).Lit(
      _.opcode -> 2.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> corrupt.B,
      _.data -> data.U
    )

    /**
      * [[LogicalData]] message is a request made by an agent that would like to access a particular block of data
      * in order to read-modify-write it with an logical operation.
      *
      * @param param   specifies the specific atomic operation to perform. It consists of [[LogicDataParam]],
      *                representing bitwise logical xor, or, and, as well as a simple swap of the operands.
      * @param size    is the operand size and reflects both the size of this request’s data as well as the
      *                [[AccessAckData]] response.
      * @param address must be aligned to `size`.
      * @param mask    provides the byte select lanes, in this case indicating which bytes to read-modify-write.
      *                One bit of `mask` corresponds to one byte of data used in the atomic operation. `size`, `address`
      *                and `mask` are required to correspond with one another (i.e., the mask is also naturally aligned
      *                and fully set HIGH contiguously within that alignment).
      * @param corrupt `corrupt` being HIGH indicates that masked data in this beat is corrupt.
      * @param data    contains one of the operands (the other is found at the target address). `data` in a byte that
      *                is unmasked is ignored and can take any value.
      * @note Supported protocol: TL-C
      **/
    def LogicalData(param: LogicDataParam,
                    size: Int,
                    source: Int,
                    address: Int,
                    mask: Int,
                    corrupt: Boolean,
                    data: BigInt): TLBundleB = new TLBundleB(edge.bundle).Lit(
      _.opcode -> 3.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> corrupt.B,
      _.data -> data.U
    )

    /**
      * [[Intent]] message is a request made by an agent that would like to signal its future intention to access a
      * particular block of data.
      *
      * @param param   specifies the specific intention being conveyed by this Hint operation. Note that its intended
      *                effect applies to the slave interface and further out in the hierarchy. It consists of [[IntentParam]],
      *                representing prefetch-data-with-intent-to-read and prefetch-data-with-intent-to-write.
      * @param size    is the size of data to which the attention applies. `address` must be aligned to `size`.
      *                `mask` provides the byte select lanes, in this case indicating the bytes to which the intention applies.
      *                `size`, `address` and `mask` are required to correspond with one another.
      * @param source  is the ID of the master that is the target of this request. It is used to route the request.
      * @param address is the address of the targeted cached block, in bytes.
      * @param mask    is byte lanes to which the Hint applies.
      * @note Supported protocol: TL-C
      **/
    def Intent(param: IntentParam,
               size: Int,
               source: Int,
               address: Int,
               mask: Int): TLBundleB = new TLBundleB(edge.bundle).Lit(
      _.opcode -> 5.U,
      _.param -> param.U,
      _.size -> size.U,
      _.source -> source.U,
      _.address -> address.U,
      _.mask -> mask.U,
      _.corrupt -> false.B,
    )

    /** [[HintAck]] serves as an acknowledgement response for a Hint operation.
      *
      * @param size    contains the size of the data that was hinted about, though this particular message contains
      *                no data itself. `address` is only required to be aligned to `size`.
      * @param address is the target address of the operation, in bytes.
      * @param source  is the ID the of the agent issuing this response message, whereas `source` should have
      *                been saved from the request and is now being re-used to route this response to the correct
      *                destination.
      * @note Supported protocol: TL-C
      * */
    def HintAck(size: Int,
                address: Int,
                source: Int): TLBundleC = new TLBundleC(edge.bundle).Lit(
      _.opcode -> 2.U,
      _.param -> 0.U,
      _.size -> size.U,
      _.address -> address.U,
      _.source -> source.U,
      _.corrupt -> false.B,
    )

  }

  implicit class AddClear[T <: TLEdge](edge: T) {
    /** clear bits of a channel.
      * @todo BundleMap not clear
      * */
    def clear(tlBundle: DecoupledIO[TLBundleBase]): TLChannel = tlBundle.bits match {
      case _: TLBundleA => clearA
      case _: TLBundleB => clearB
      case _: TLBundleC => clearC
      case _: TLBundleD => clearD
      case _: TLBundleE => clearE
    }

    private def clearA: TLBundleA = new TLBundleA(edge.bundle).Lit(
      _.opcode -> 0.U,
      _.param -> 0.U,
      _.size -> 0.U,
      _.source -> 0.U,
      _.address -> 0.U,
      _.mask -> 0.U,
      _.data -> 0.U,
      _.corrupt -> false.B
    )

    private def clearB: TLBundleB = new TLBundleB(edge.bundle).Lit(
      _.opcode -> 0.U,
      _.param -> 0.U,
      _.size -> 0.U,
      _.source -> 0.U,
      _.address -> 0.U,
      _.mask -> 0.U,
      _.data -> 0.U,
      _.corrupt -> false.B
    )

    private def clearC: TLBundleC = new TLBundleC(edge.bundle).Lit(
      _.opcode -> 0.U,
      _.param -> 0.U,
      _.size -> 0.U,
      _.source -> 0.U,
      _.address -> 0.U,
      _.data -> 0.U,
      _.corrupt -> false.B
    )

    private def clearD: TLBundleD = new TLBundleD(edge.bundle).Lit(
      _.opcode -> 0.U,
      _.param -> 0.U,
      _.size -> 0.U,
      _.source -> 0.U,
      _.sink -> 0.U,
      _.denied -> false.B,
      _.data -> 0.U,
      _.corrupt -> false.B
    )

    private def clearE: TLBundleE = new TLBundleE(edge.bundle).Lit(
      _.sink -> 0.U
    )
  }

  type ArithmeticDataParam = Int

  object ArithmeticDataParam {
    /** Write the signed minimum of the two operands, and return the old value. */
    val MIN: ArithmeticDataParam = 0
    /** Write the signed maximum of the two operands, and return the old value. */
    val MAX: ArithmeticDataParam = 1
    /** Write the unsigned minimum of the two operands, and return the old value. */
    val MINU: ArithmeticDataParam = 2
    /** Write the unsigned maximum of the two operands, and return the old value. */
    val MAXU: ArithmeticDataParam = 3
    /** Write the sum of the two operands, and return the old value. */
    val ADD: ArithmeticDataParam = 4
  }

  type LogicDataParam = Int

  object LogicDataParam {
    /** Bitwise logical xor the two operands, write the result, and return the old value. */
    val XOR: LogicDataParam = 0
    /** Bitwise logical or the two operands, write the result, and return the old value. */
    val OR: LogicDataParam = 1
    /** Bitwise logical and the two operands, write the result, and return the old value. */
    val AND: LogicDataParam = 2
    /** Swap the two operands and return the old value. */
    val SWAP: LogicDataParam = 3
  }

  type IntentParam = Int

  object IntentParam {
    /** Issuing agent intends to read target data. */
    val PrefetchRead: IntentParam = 0
    /** Issuing agent intends to write target data. */
    val PrefetchWrite: IntentParam = 1
  }

  type Permission = Int

  object Cap {
    val toT: Permission = 0
    val toB: Permission = 1
    val toN: Permission = 2
  }

  object Grow {
    val NtoB: Permission = 0
    val NtoT: Permission = 1
    val BtoT: Permission = 2
  }

  object Shrink {
    val TtoB: Permission = 0
    val TtoN: Permission = 1
    val BtoN: Permission = 2
  }

  object Report {
    val TtoT: Permission = 3
    val BtoB: Permission = 3
    val NtoN: Permission = 3
  }

}


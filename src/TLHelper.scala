package diplomatictester

import freechips.rocketchip.tilelink.{TLEdgeIn, TLEdgeOut}

object TLHelper {
  def flip(edge: TLEdgeIn): TLEdgeOut = new TLEdgeOut(edge.client, edge.manager, edge.params, edge.sourceInfo)

  def flip(edge: TLEdgeOut): TLEdgeIn = new TLEdgeIn(edge.client, edge.manager, edge.params, edge.sourceInfo)
}

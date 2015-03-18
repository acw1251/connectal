/* Copyright (c) 2014 Quanta Research Cambridge, Inc
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
import SpecialFIFOs::*;
import Vector::*;
import StmtFSM::*;
import FIFO::*;
import CtrlMux::*;
import Portal::*;
import HostInterface::*;
import ConnectalMemory::*;
import DmaUtils::*;
import MemServer::*;
import MMU::*;
import RingIndication::*;
import RingRequest::*;
import MemServerRequest::*;
import MMURequest::*;
import MemServerIndication::*;
import MMUIndication::*;
import Ring::*;

typedef enum {RingIndication, RingRequest, HostMemServerIndication, HostMemServerRequest, HostMMURequest, HostMMUIndication} IfcNames deriving (Eq,Bits);

module mkConnectalTop(StdConnectalDmaTop#(PhysAddrWidth));
   DmaReadBuffer#(64,8) dma_read_chan <- mkDmaReadBuffer();
   DmaWriteBuffer#(64,8) dma_write_chan <- mkDmaWriteBuffer();
   DmaReadBuffer#(64,8) cmd_read_chan <- mkDmaReadBuffer();
   DmaWriteBuffer#(64,8) cmd_write_chan <- mkDmaWriteBuffer();
   
   Vector#(2, MemReadClient#(64)) readClients = newVector();
   readClients[0] = dma_read_chan.dmaClient;
   readClients[1] = cmd_read_chan.dmaClient;

   Vector#(2, MemWriteClient#(64)) writeClients = newVector();
   writeClients[0] = dma_write_chan.dmaClient;
   writeClients[1] = cmd_write_chan.dmaClient;

   MMUIndicationProxy hostMMUIndicationProxy <- mkMMUIndicationProxy(HostMMUIndication);
   MMU#(PhysAddrWidth) hostMMU <- mkMMU(0, True, hostMMUIndicationProxy.ifc);
   MMURequestWrapper hostMMURequestWrapper <- mkMMURequestWrapper(HostMMURequest, hostMMU.request);

   MemServerIndicationProxy hostMemServerIndicationProxy <- mkMemServerIndicationProxy(HostMemServerIndication);
   MemServer#(PhysAddrWidth,64,1) dma <- mkMemServer(readClients, writeClients, cons(hostMMU,nil), hostMemServerIndicationProxy.ifc);
   MemServerRequestWrapper hostMemServerRequestWrapper <- mkMemServerRequestWrapper(HostMemServerRequest, dma.request);
   
   RingIndicationProxy ringIndicationProxy <- mkRingIndicationProxy(RingIndication);
   RingRequest ringRequest <- mkRingRequest(ringIndicationProxy.ifc, dma_read_chan.dmaServer, dma_write_chan.dmaServer, cmd_read_chan.dmaServer, cmd_write_chan.dmaServer);
   RingRequestWrapper ringRequestWrapper <- mkRingRequestWrapper(RingRequest, ringRequest);
   
   Vector#(6,StdPortal) portals;
   portals[0] = ringIndicationProxy.portalIfc;
   portals[1] = ringRequestWrapper.portalIfc; 
   portals[2] = hostMemServerIndicationProxy.portalIfc;
   portals[3] = hostMemServerRequestWrapper.portalIfc; 
   portals[4] = hostMMURequestWrapper.portalIfc;
   portals[5] = hostMMUIndicationProxy.portalIfc;
   let ctrl_mux <- mkSlaveMux(portals);

   interface interrupt = getInterruptVector(portals);
   interface slave = ctrl_mux;
   interface masters = dma.masters;
endmodule : mkConnectalTop

// Copyright (c) 2013 Quanta Research Cambridge, Inc.

// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
import Vector::*;
import MemServer::*;
import MMU::*;
import Portal::*;
import HostInterface::*;
import CtrlMux::*;
import MemTypes::*;
import RtestRequest::*;
import MemServerRequest::*;
import MMURequest::*;
import RtestIndication::*;
import MemServerIndication::*;
import MMUIndication::*;

// defined by user
import Rtest::*;

typedef enum {RtestIndication, RtestRequest, HostMemServerIndication, HostMemServerRequest, HostMMURequest, HostMMUIndication} IfcNames deriving (Eq,Bits);

module mkConnectalTop(StdConnectalDmaTop#(PhysAddrWidth));

   RtestIndicationProxy memreadIndicationProxy <- mkRtestIndicationProxy(RtestIndication);
   Rtest memread <- mkRtest(memreadIndicationProxy.ifc);
   RtestRequestWrapper memreadRequestWrapper <- mkRtestRequestWrapper(RtestRequest,memread.request);

   Vector#(1, MemReadClient#(64)) readClients = cons(memread.dmaClient, nil);

   MMUIndicationProxy hostMMUIndicationProxy <- mkMMUIndicationProxy(HostMMUIndication);
   MemServerIndicationProxy hostMemServerIndicationProxy <- mkMemServerIndicationProxy(HostMemServerIndication);

   SimpleMemServer#(PhysAddrWidth,64,1) dma <- mkSimpleMemServer(readClients, nil, hostMemServerIndicationProxy.ifc, hostMMUIndicationProxy.ifc);

   MMURequestWrapper hostMMURequestWrapper <- mkMMURequestWrapper(HostMMURequest, dma.mmuRequest);
   MemServerRequestWrapper hostMemServerRequestWrapper <- mkMemServerRequestWrapper(HostMemServerRequest, dma.memServerRequest);

   Vector#(6,StdPortal) portals;
   portals[0] = hostMemServerIndicationProxy.portalIfc; 
   portals[1] = memreadIndicationProxy.portalIfc; 
   portals[2] = hostMemServerRequestWrapper.portalIfc;
   portals[3] = memreadRequestWrapper.portalIfc;
   portals[4] = hostMMURequestWrapper.portalIfc;
   portals[5] = hostMMUIndicationProxy.portalIfc;
   let ctrl_mux <- mkSlaveMux(portals);
   
   interface interrupt = getInterruptVector(portals);
   interface slave = ctrl_mux;
   interface masters = dma.masters;
endmodule : mkConnectalTop

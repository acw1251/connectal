// bsv libraries
import SpecialFIFOs::*;
import Vector::*;
import StmtFSM::*;
import FIFO::*;
import BRAM::*;
import DefaultValue::*;
import Connectable::*;

// portz libraries
import Leds::*;
import CtrlMux::*;
import Portal::*;
import ConnectalMemory::*;
import MemTypes::*;
import MemServer::*;
import MemUtils::*;
import MMU::*;

// generated by tool
import NandSimRequest::*;
import DmaDebugRequest::*;
import MMUConfigRequest::*;
import NandSimIndication::*;
import DmaDebugIndication::*;
import MMUConfigIndication::*;

// defined by user
import NandSim::*;
import NandSimNames::*;

module mkConnectalTop(StdConnectalDmaTop#(PhysAddrWidth));
   
   NandSimIndicationProxy nandSimIndicationProxy <- mkNandSimIndicationProxy(NandSimIndication);   
   NandSim nandSim <- mkNandSim(nandSimIndicationProxy.ifc);
   NandSimRequestWrapper nandSimRequestWrapper <- mkNandSimRequestWrapper(NandSimRequest,nandSim.request);
   
   MMUConfigIndicationProxy backingStoreMMUConfigIndicationProxy <- mkMMUConfigIndicationProxy(BackingStoreMMUConfigIndication);
   MMU#(PhysAddrWidth) backingStoreSGList <- mkMMU(0, True, backingStoreMMUConfigIndicationProxy.ifc);
   MMUConfigRequestWrapper backingStoreMMUConfigRequestWrapper <- mkMMUConfigRequestWrapper(BackingStoreMMUConfigRequest, backingStoreSGList.request);

   DmaDebugIndicationProxy hostDmaDebugIndicationProxy <- mkDmaDebugIndicationProxy(HostDmaDebugIndication);
   MemServer#(PhysAddrWidth,64,1) hostDma <- mkMemServerRW(backingStoreMMUConfigIndicationProxy.ifc, hostDmaDebugIndicationProxy.ifc, cons(nandSim.readClient, nil), cons(nandSim.writeClient, nil), cons(backingStoreSGList, nil));
   DmaDebugRequestWrapper hostDmaDebugRequestWrapper <- mkDmaDebugRequestWrapper(HostDmaDebugRequest, hostDma.request);
   
   
   Vector#(6,StdPortal) portals;
   portals[0] = nandSimRequestWrapper.portalIfc;
   portals[1] = nandSimIndicationProxy.portalIfc; 
   portals[2] = hostDmaDebugRequestWrapper.portalIfc;
   portals[3] = hostDmaDebugIndicationProxy.portalIfc; 
   portals[4] = backingStoreMMUConfigRequestWrapper.portalIfc;
   portals[5] = backingStoreMMUConfigIndicationProxy.portalIfc;
   let ctrl_mux <- mkSlaveMux(portals);
   
   interface interrupt = getInterruptVector(portals);
   interface slave = ctrl_mux;
   interface masters = hostDma.masters;
   interface leds = default_leds;
      
endmodule : mkConnectalTop

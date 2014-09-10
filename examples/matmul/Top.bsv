// bsv libraries
import SpecialFIFOs::*;
import Vector::*;
import StmtFSM::*;
import FIFO::*;
import Connectable::*;

// portz libraries
import Directory::*;
import CtrlMux::*;
import Portal::*;
import Leds::*;
import PortalMemory::*;
import MemServer::*;
import MMU::*;
import PortalMemory::*;
import MemTypes::*;
import RbmTypes::*;
import HostInterface::*;

// generated by tool
import DmaDebugRequestWrapper::*;
import MMUConfigRequestWrapper::*;
import DmaDebugIndicationProxy::*;
import MMUConfigIndicationProxy::*;
import MmIndicationProxy::*;
import TimerIndicationProxy::*;
import TimerRequestWrapper::*;

`ifdef MATRIX_TN
import MmRequestTNWrapper::*;
import MatrixTN::*;
`else
`ifdef MATRIX_NT
import MmRequestNTWrapper::*;
import MatrixNT::*;
`endif
`endif

module  mkPortalTop#(HostType host)(PortalTop#(PhysAddrWidth,TMul#(32,N),Empty,NumberOfMasters));

   MmIndicationProxy mmIndicationProxy <- mkMmIndicationProxy(MmIndicationPortal);
   TimerIndicationProxy timerIndicationProxy <- mkTimerIndicationProxy(TimerIndicationPortal);
`ifdef MATRIX_TN
   MmTN#(N) mm <- mkMmTN(mmIndicationProxy.ifc, timerIndicationProxy.ifc, host);
   MmRequestTNWrapper mmRequestWrapper <- mkMmRequestTNWrapper(MmRequestPortal,mm.mmRequest);
`else
`ifdef MATRIX_NT
   MmNT#(N) mm <- mkMmNT(mmIndicationProxy.ifc, timerIndicationProxy.ifc, host);
   MmRequestNTWrapper mmRequestWrapper <- mkMmRequestNTWrapper(MmRequestPortal,mm.mmRequest);
`endif
`endif
   TimerRequestWrapper timerRequestWrapper <- mkTimerRequestWrapper(TimerRequestPortal,mm.timerRequest);
   
   Vector#(2,ObjectReadClient#(TMul#(32,N)))  readClients  = mm.readClients;
   Vector#(2,ObjectWriteClient#(TMul#(32,N))) writeClients = mm.writeClients;

   MMUConfigIndicationProxy hostMMUConfigIndicationProxy <- mkMMUConfigIndicationProxy(HostMMUConfigIndication);
   MMU#(PhysAddrWidth) hostMMU <- mkMMU(0, True, hostMMUConfigIndicationProxy.ifc);
   MMUConfigRequestWrapper hostMMUConfigRequestWrapper <- mkMMUConfigRequestWrapper(HostMMUConfigRequest, hostMMU.request);

   DmaDebugIndicationProxy hostDmaDebugIndicationProxy <- mkDmaDebugIndicationProxy(HostDmaDebugIndication);
   MemServer#(PhysAddrWidth, TMul#(32,N), NumberOfMasters) dma <- mkMemServerRW(hostDmaDebugIndicationProxy.ifc, readClients, writeClients, cons(hostMMU,nil));
   DmaDebugRequestWrapper hostDmaDebugRequestWrapper <- mkDmaDebugRequestWrapper(HostDmaDebugRequest, dma.request);

   Vector#(8,StdPortal) portals;
   portals[0] = mmRequestWrapper.portalIfc;
   portals[1] = mmIndicationProxy.portalIfc; 
   portals[2] = hostDmaDebugRequestWrapper.portalIfc;
   portals[3] = hostDmaDebugIndicationProxy.portalIfc; 
   portals[4] = timerRequestWrapper.portalIfc;
   portals[5] = timerIndicationProxy.portalIfc; 
   portals[6] = hostMMUConfigRequestWrapper.portalIfc;
   portals[7] = hostMMUConfigIndicationProxy.portalIfc;

   StdDirectory dir <- mkStdDirectory(portals);
   let ctrl_mux <- mkSlaveMux(dir,portals);
   
   interface interrupt = getInterruptVector(portals);
   interface slave = ctrl_mux;
   interface masters = dma.masters;

endmodule : mkPortalTop

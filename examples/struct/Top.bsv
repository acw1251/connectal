// bsv libraries
import Vector::*;
import FIFO::*;
import Connectable::*;

// portz libraries
import Directory::*;
import CtrlMux::*;
import Portal::*;
import Leds::*;

// generated by tool
import StructIndicationProxy::*;
import StructRequestWrapper::*;

// defined by user
import Struct::*;

module mkPortalTop(StdPortalTop);

   // instantiate user portals
   StructIndicationProxy structIndicationProxy <- mkStructIndicationProxy(7);
   StructRequest structRequest <- mkStructRequest(structIndicationProxy.ifc);
   StructRequestWrapper structRequestWrapper <- mkStructRequestWrapper(1008,structRequest);
   
   Vector#(2,StdPortal) portals;
   portals[0] = structIndicationProxy.portalIfc;
   portals[1] = structRequestWrapper.portalIfc; 
   let interrupt_mux <- mkInterruptMux(portals);
   
   // instantiate system directory
   Directory dir <- mkDirectoryDbg(portals,interrupt_mux);
   Vector#(1,StdPortal) directories;
   directories[0] = dir.portalIfc;
   
   // when constructing the ctrl mux, directories must be the first argument
   let ctrl_mux <- mkAxiSlaveMux(directories,portals);
   
   interface ReadOnly interrupt = interrupt_mux;
   interface StdAxi3Slave ctrl = ctrl_mux;
   interface Vector m_axi = ?;
   interface LEDS leds = ?;

endmodule : mkPortalTop



// Copyright (c) 2014 Quanta Research Cambridge, Inc.

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
import Vector            :: *;
import Clocks            :: *;
import GetPut            :: *;
import FIFO              :: *;
import Connectable       :: *;
import ClientServer      :: *;
import DefaultValue      :: *;

import ConnectalConfig::*;
`include "ConnectalProjectConfig.bsv"
import PcieSplitter      :: *;
import Xilinx            :: *;
import Portal            :: *;
import Top               :: *;
import PcieCsr           :: *;
import MemTypes          :: *;
import Bscan             :: *;
import ConnectalClocks   :: *;
import GetPutWithClocks  :: *;
`ifdef XILINX
`ifdef PCIE3
import PCIEWRAPPER3      :: *;
import Pcie3EndpointX7   :: *;
`endif
`ifdef PCIE2
import PCIEWRAPPER2       :: *;
import Pcie2EndpointX7 :: *;
`endif // pcie2
`ifdef PCIE1
import PCIEWRAPPER       :: *;
import Pcie1EndpointX7   :: *;
`endif // pcie2
`elsif ALTERA
import PcieEndpointS5    :: *;
`endif
import PcieHost          :: *;
import HostInterface     :: *;
import `PinTypeInclude::*;
import Platform          :: *;
import AddImplicitCondition::*;

`ifndef DataBusWidth
`define DataBusWidth 64
`endif

`ifdef XILINX_SYS_CLK
`define SYS_CLK_PARAM Clock sys_clk_p, Clock sys_clk_n,
`define SYS_CLK_ARG sys_clk_p, sys_clk_n,
`else
`define SYS_CLK_PARAM
`define SYS_CLK_ARG
`endif

interface PhysMemFIFO#(numeric type asz, numeric type dsz);
    interface PhysMemSlave#(asz,dsz) slave;
    interface PhysMemMaster#(asz,dsz) master;
endinterface

module mkPhysMemFIFO(PhysMemFIFO#(asz,dsz));
    FIFO#(PhysMemRequest#(asz, dsz)) writeReqFIFO  <- mkFIFO;
    FIFO#(MemData#(dsz))             writeDataFIFO <- mkFIFO;
    FIFO#(Bit#(MemTagSize))          writeDoneFIFO <- mkFIFO;
    FIFO#(PhysMemRequest#(asz, dsz)) readReqFIFO   <- mkFIFO;
    FIFO#(MemData#(dsz))             readDataFIFO  <- mkFIFO;
    interface PhysMemSlave slave;
        interface PhysMemReadServer read_server;
            interface Put readReq = toPut(readReqFIFO);
            interface Get readData = toGet(readDataFIFO);
        endinterface
        interface PhysMemWriteServer write_server;
            interface Put writeReq = toPut(writeReqFIFO);
            interface Put writeData = toPut(writeDataFIFO);
            interface Get writeDone = toGet(writeDoneFIFO);
        endinterface
    endinterface
    interface PhysMemMaster master;
        interface PhysMemReadClient read_client;
            interface Put readReq = toGet(readReqFIFO);
            interface Get readData = toPut(readDataFIFO);
        endinterface
        interface PhysMemWriteClient write_client;
            interface Get writeReq = toGet(writeReqFIFO);
            interface Get writeData = toGet(writeDataFIFO);
            interface Put writeDone = toPut(writeDoneFIFO);
        endinterface
    endinterface
endmodule

(* synthesize, no_default_clock, no_default_reset *)
`ifdef XILINX
module mkPcieTop #(Clock pci_sys_clk_p, Clock pci_sys_clk_n, `SYS_CLK_PARAM Reset pci_sys_reset_n) (PcieTop#(`PinType));
   PcieHostTop host <- mkPcieHostTop(pci_sys_clk_p, pci_sys_clk_n, `SYS_CLK_ARG pci_sys_reset_n);
`elsif ALTERA
(* clock_prefix="", reset_prefix="" *)
module mkPcieTop #(Clock pcie_refclk_p, Clock osc_50_b3b, Reset pcie_perst_n) (PcieTop#(`PinType));
   PcieHostTop host <- mkPcieHostTop(pcie_refclk_p, osc_50_b3b, pcie_perst_n);
`endif

   Vector#(NumberOfUserTiles,ConnectalTop#(`PinType)) tile <- replicateM(mkConnectalTop(
`ifdef IMPORT_HOSTIF // no synthesis boundary
      host,
`else                // enables synthesis boundary
`ifdef IMPORT_HOST_CLOCKS
       host.derivedClock, host.derivedReset,
`endif
`endif
       // clocked_by host.portalClock, reset_by host.portalReset));
       clocked_by host.pcieClock, reset_by host.pcieReset));
   // Platform portalTop <- mkPlatform(tile, clocked_by host.portalClock, reset_by host.portalReset);
   Platform portalTop <- mkPlatform(tile, clocked_by host.pcieClock, reset_by host.pcieReset);

   PhysMemFIFO#(32,32) portalSlaveFIFO <- mkPhysMemFIFO(clocked_by host.portalClock, reset_by host.portalReset);
   Vector#(NumberOfMasters, PhysMemFIFO#(40,64)) portalMasterFIFOs <- replicateM(mkPhysMemFIFO(clocked_by host.portalClock, reset_by host.portalReset));
   function PhysMemMaster#(asz, dsz) getMaster(PhysMemFIFO#(asz, dsz) fifo);
       return fifo.master;
   endfunction
   function PhysMemSlave#(asz, dsz) getSlave(PhysMemFIFO#(asz, dsz) fifo);
       return fifo.slave;
   endfunction

   // if (mainClockPeriod == pcieClockPeriod) begin

       mkConnection(host.tpciehost.master, portalTop.slave, clocked_by host.pcieClock, reset_by host.pcieReset);
       // mkConnectionWithClocks2( host.tpciehost.master, portalSlaveFIFO.slave);
       // mkConnectionWithClocks2( portalSlaveFIFO.master, portalTop.slave );

       if (valueOf(NumberOfMasters) > 0) begin
	  // zipWithM_(mkConnection, portalTop.masters, host.tpciehost.slave);
          zipWithM_(mkConnectionWithClocks2, portalTop.masters, map(getSlave, portalMasterFIFOs));
          zipWithM_(mkConnectionWithClocks2, map(getMaster, portalMasterFIFOs), host.tpciehost.slave);
       end

   // end
   // else begin
   //     let portalCnx <- GetPutWithClocks::mkConnectionWithClocks(host.pcieClock, host.pcieReset,
   //      							 host.portalClock, host.portalReset,
   //      							 host.tpciehost.master, portalTop.slave);
   //     if (valueOf(NumberOfMasters) > 0) begin
   //        //zipWithM_(GetPutWithClocks::mkConnectionWithClocks2, portalTop.masters, host.tpciehost.slave);
   //        for (Integer i = 0; i < valueOf(NumberOfMasters); i = i + 1)
   //           let memCnx <- GetPutWithClocks::mkConnectionWithClocks(host.portalClock, host.portalReset,
   //      							    host.pcieClock, host.pcieReset,
   //      							    portalTop.masters[i], host.tpciehost.slave[i]);
   //     end
   // end

   // going from level to edge-triggered interrupt
   // FIFO#(Bit#(4)) intrFifo <- mkFIFO(clocked_by host.portalClock, reset_by host.portalReset);
   FIFO#(Bit#(4)) intrFifo <- mkFIFO(clocked_by host.pcieClock, reset_by host.pcieReset);
   //(8, host.portalClock, host.portalReset, host.pcieClock);
   // Vector#(16, Reg#(Bool)) interruptRequested <- replicateM(mkReg(False, clocked_by host.portalClock, reset_by host.portalReset));
   Vector#(16, Reg#(Bool)) interruptRequested <- replicateM(mkReg(False, clocked_by host.pcieClock, reset_by host.pcieReset));
   rule interrupt_rule;
     Maybe#(Bit#(4)) intr = tagged Invalid;
     for (Integer i = 0; i < 16; i = i + 1) begin
	 if (portalTop.interrupt[i] && !interruptRequested[i])
             intr = tagged Valid fromInteger(i);
	 interruptRequested[i] <= portalTop.interrupt[i];
     end
     if (intr matches tagged Valid .intr_num) begin
	intrFifo.enq(intr_num);
     end
   endrule
   Put#(Bit#(4)) intrPut = (interface Put;
      method Action put(Bit#(4) intr_num);
	ReadOnly_MSIX_Entry msixEntry = host.tpciehost.msixEntry[intr_num];
	host.tpciehost.interruptRequest.put(tuple2({msixEntry.addr_hi, msixEntry.addr_lo}, msixEntry.msg_data));
      endmethod
      endinterface);

   // GetPutWithClocks::mkConnectionWithClocks(host.portalClock, host.portalReset,
   GetPutWithClocks::mkConnectionWithClocks(host.pcieClock, host.pcieReset,
					    host.pcieClock, host.pcieReset,
					    toGet(intrFifo),
					    intrPut);

   interface pcie = host.tep7.pcie;
   interface pins = portalTop.pins;
endmodule


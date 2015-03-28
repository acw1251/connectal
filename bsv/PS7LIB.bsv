
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

import Clocks::*;
import DefaultValue::*;
import GetPut::*;
import Connectable::*;
import ConnectableWithTrace::*;
import Bscan::*;
import Vector::*;
import PPS7LIB::*;
//import CtrlMux::*;
import Portal::*;
import AxiMasterSlave::*;
import AxiDma::*;
import XilinxCells::*;
import ConnectalXilinxCells::*;
import ConnectalClocks::*;
import AxiGather::*;

(* always_ready, always_enabled *)
interface Bidir#(numeric type data_width);
    method Action             i(Bit#(data_width) v);
    method Bit#(data_width)   o();
    method Bit#(data_width)   t();
endinterface

interface PS7LIB;
`ifdef PS7EXTENDED
    interface Vector#(2, Pps7Can)  can;
    interface Vector#(4, Pps7Dma)  dma;
    interface Vector#(2, Pps7Enet) enet;
    interface Pps7Event            event_;
    interface Vector#(4,Pps7Fclk_clktrig)fclk_clktrig;
    interface Pps7Fpga             fpga;
    interface Pps7Ftmd             ftmd;
    interface Pps7Ftmt             ftmt;
    interface Pps7Pjtag            pjtag;
    interface Vector#(2, Pps7Sdio) sdio;
    interface Vector#(2, Pps7Spi)  spi;
    interface Pps7Sram             sram;
    interface Pps7Trace            trace;
    interface Vector#(2, Pps7Ttc)  ttc;
    interface Vector#(2, Pps7Uart) uart;
    interface Vector#(2, Pps7Usb)  usb;
    interface Pps7Wdt              wdt;
`endif
    interface Pps7Ddr              ddr;
    method Bit#(4)     fclkclk();
    method Action      fclkclktrign(Bit#(4) v);
    method Bit#(4)     fclkresetn();
    method Action      fpgaidlen(Bit#(1) v);
    interface Pps7Emiogpio             gpio;
    interface Vector#(2, Pps7Emioi2c)  i2c;
    interface Pps7Irq              irq;
    interface Inout#(Bit#(54))     mio;
    interface Pps7Ps               ps;

    interface Vector#(2, AxiMasterCommon) m_axi_gp;
    interface Vector#(2, AxiSlaveCommon#(32,6)) s_axi_gp;
    interface Vector#(4, AxiSlaveHighSpeed) s_axi_hp;
    interface AxiSlaveCommon#(64,3) s_axi_acp;
endinterface

module mkPS7LIB#(Clock axi_clock, Reset axi_reset)(PS7LIB);
    PPS7LIB foo <- mkPPS7LIB(
        axi_clock, axi_reset, axi_clock, axi_reset, axi_clock, axi_reset, axi_clock, axi_reset,
        axi_clock, axi_reset, axi_clock, axi_reset, axi_clock, axi_reset, axi_clock, axi_reset,
        axi_clock, axi_reset);
`ifdef PS7EXTENDED
    Vector#(2, Pps7Can)     vcan;
    Vector#(4, Pps7Dma)     vdma;
    Vector#(2, Pps7Enet)     venet;
    Vector#(2, Pps7Sdio)     vsdio;
    Vector#(2, Pps7Spi)     vspi;
    Vector#(2, Pps7Ttc)     vttc;
    Vector#(2, Pps7Uart)     vuart;
    Vector#(2, Pps7Usb)     vusb;
`endif
    Vector#(2, Pps7Emioi2c)     vi2c;
    Vector#(1, Pps7Saxiacp)    vs_axi_acp;
    Vector#(2, AxiMasterCommon) vtopm_axi_gp;
    //Vector#(2, AxiMasterWires) vtopmw_axi_gp <- replicateM(mkAxiMasterWires(clocked_by axi_clock, reset_by axi_reset));
    Vector#(2, AxiSlaveCommon#(32,6)) vtops_axi_gp;
    Vector#(1, AxiSlaveCommon#(64,3)) vtops_axi_acp;
    //Vector#(2, AxiSlaveWires#(32,6)) vtopsw_axi_gp <- replicateM(mkAxiSlaveWires(clocked_by axi_clock, reset_by axi_reset));
    Vector#(4, AxiSlaveHighSpeed) vtops_axi_hp;
    //Vector#(4, AxiSlaveWires#(64,6)) vtopsw_axi_hp <- replicateM(mkAxiSlaveWires(clocked_by axi_clock, reset_by axi_reset));
    //Vector#(1, AxiSlaveWires#(64,3)) vtopsw_axi_acp <- replicateM(mkAxiSlaveWires(clocked_by axi_clock, reset_by axi_reset));

`ifdef PS7EXTENDED
    vcan[0] = foo.can0;
    vcan[1] = foo.can1;
    vdma[0] = foo.dma0;
    vdma[1] = foo.dma1;
    vdma[2] = foo.dma2;
    vdma[3] = foo.dma3;
    venet[0] = foo.enet0;
    venet[1] = foo.enet1;
    vsdio[0] = foo.sdio0;
    vsdio[1] = foo.sdio1;
    vspi[0] = foo.spi0;
    vspi[1] = foo.spi1;
    vttc[0] = foo.ttc0;
    vttc[1] = foo.ttc1;
    vuart[0] = foo.uart0;
    vuart[1] = foo.uart1;
    vusb[0] = foo.usb0;
    vusb[1] = foo.usb1;
`endif
    vi2c[0] = foo.emioi2c0;
    vi2c[1] = foo.emioi2c1;
    vtopm_axi_gp[0] <- mkAxi3MasterGather(foo.maxigp0, clocked_by axi_clock, reset_by axi_reset);
    vtopm_axi_gp[1] <- mkAxi3MasterGather(foo.maxigp1, clocked_by axi_clock, reset_by axi_reset);
    vtops_axi_gp[0] <- mkAxi3SlaveGather(foo.saxigp0, clocked_by axi_clock, reset_by axi_reset);
    vtops_axi_gp[1] <- mkAxi3SlaveGather(foo.saxigp1, clocked_by axi_clock, reset_by axi_reset);
    vtops_axi_acp[0] <- mkAxi3SlaveGather64(foo.saxiacp, clocked_by axi_clock, reset_by axi_reset);
    vtops_axi_hp[0] <- mkAxiSlaveHighSpeedGather(foo.saxihp0, clocked_by axi_clock, reset_by axi_reset);
    vtops_axi_hp[1] <- mkAxiSlaveHighSpeedGather(foo.saxihp1, clocked_by axi_clock, reset_by axi_reset);
    vtops_axi_hp[2] <- mkAxiSlaveHighSpeedGather(foo.saxihp2, clocked_by axi_clock, reset_by axi_reset);
    vtops_axi_hp[3] <- mkAxiSlaveHighSpeedGather(foo.saxihp3, clocked_by axi_clock, reset_by axi_reset);
   Wire#(Bit#(1)) fpgaidlenw <- mkDWire(1);
   rule fpgaidle;
      foo.fpgaidlen(fpgaidlenw);
   endrule
   rule misc;
      foo.emiosramintin(0);
      // UG585 "fclkclktrign is currently not supported and must be tied to ground"
      foo.fclkclktrign(0);
   endrule

`ifdef PS7EXTENDED
    interface Pps7Can can = vcan;
    interface Pps7Dma     dma = vdma;
    interface Pps7Enet     enet = venet;
    interface Pps7Sdio    sdio = vsdio;
    interface Pps7Spi    spi = vspi;
    interface Pps7Ttc    ttc = vttc;
    interface Pps7Uart    uart = vuart;
    interface Pps7Usb    usb = vusb;
    interface Pps7Event     event_ = foo.event_;
    interface Pps7Fpga     fpga = foo.fpga;
    interface Pps7Ftmd     ftmd = foo.ftmd;
    interface Pps7Ftmt     ftmt = foo.ftmt;
    interface Pps7Pjtag     pjtag = foo.pjtag;
    interface Pps7Sram     sram = foo.sram;
    interface Pps7Trace     trace = foo.trace;
    interface Pps7Wdt     wdt = foo.wdt;
`endif
    interface Pps7Emioi2c    i2c = vi2c;
    interface Pps7Ddr     ddr = foo.ddr;
    interface Bit        fclkclk = foo.fclkclk;
    interface Bit        fclkresetn = foo.fclkresetn;
    method Action      fclkclktrign(Bit#(4) v);
        foo.fclkclktrign(v);
    endmethod
    method Action      fpgaidlen(Bit#(1) v);
       fpgaidlenw <= v;
    endmethod
    interface Pps7Emiogpio     gpio = foo.emiogpio;
    interface Pps7Irq     irq = foo.irq;
    interface Inout     mio = foo.mio;
    interface Pps7Ps     ps = foo.ps;

    interface AxiMasterCommon m_axi_gp = vtopm_axi_gp;
    interface AxiSlaveCommon s_axi_gp = vtops_axi_gp;
    interface AxiSlaveHighSpeed s_axi_hp = vtops_axi_hp;
    interface AxiSlaveCommon s_axi_acp = vtops_axi_acp[0];
endmodule

interface ZynqPins;
    (* prefix="DDR_Addr" *) interface Inout#(Bit#(15))     a;
    (* prefix="DDR_BankAddr" *) interface Inout#(Bit#(3))     ba;
    (* prefix="DDR_CAS_n" *) interface Inout#(Bit#(1))     casb;
    (* prefix="DDR_CKE" *) interface Inout#(Bit#(1))     cke;
    (* prefix="DDR_CS_n" *) interface Inout#(Bit#(1))     csb;
    (* prefix="DDR_Clk_n" *) interface Inout#(Bit#(1))     ckn;
    (* prefix="DDR_Clk_p" *) interface Inout#(Bit#(1))     ck;
    (* prefix="DDR_DM" *) interface Inout#(Bit#(4))     dm;
    (* prefix="DDR_DQ" *) interface Inout#(Bit#(32))     dq;
    (* prefix="DDR_DQS_n" *) interface Inout#(Bit#(4))     dqsn;
    (* prefix="DDR_DQS_p" *) interface Inout#(Bit#(4))     dqs;
    (* prefix="DDR_DRSTB" *) interface Inout#(Bit#(1))     drstb;
    (* prefix="DDR_ODT" *) interface Inout#(Bit#(1))     odt;
    (* prefix="DDR_RAS_n" *) interface Inout#(Bit#(1))     rasb;
    (* prefix="FIXED_IO_ddr_vrn" *) interface Inout#(Bit#(1))     vrn;
    (* prefix="FIXED_IO_ddr_vrp" *) interface Inout#(Bit#(1))     vrp;
    (* prefix="DDR_WEB" *) interface Inout#(Bit#(1))     web;
    (* prefix="FIXED_IO_mio" *)
    interface Inout#(Bit#(54))       mio;
    (* prefix="FIXED_IO_ps" *)
    interface Pps7Ps ps;
endinterface

interface PS7;
    (* prefix="" *)
    interface ZynqPins pins;
    interface Vector#(2, AxiMasterCommon)     m_axi_gp;
    interface Vector#(2, AxiSlaveCommon#(32,6)) s_axi_gp;
    interface Vector#(4, AxiSlaveHighSpeed)   s_axi_hp;
    method Action                             interrupt(Bit#(1) v);
    interface Vector#(4, Clock) fclkclk;
    interface Vector#(4, Reset) fclkreset;
    interface Vector#(2, Pps7Emioi2c)  i2c;
    interface Clock derivedClock;
    interface Reset derivedReset;
endinterface

module mkPS7(PS7);
   // B2C converts a bit to a clock, enabling us to break the apparent cycle
   Vector#(4, B2C) b2c <- replicateM(mkB2C());

   // need the bufg here to reduce clock skew
   module mkBufferedClock#(Integer i)(Clock); let c <- mkClockBUFG(clocked_by b2c[i].c); return c; endmodule
   module mkBufferedReset#(Integer i)(Reset); let r <- mkResetBUFG(clocked_by b2c[i].c, reset_by b2c[i].r); return r; endmodule
   Vector#(4, Clock) fclk <- genWithM(mkBufferedClock);
   Vector#(4, Reset) freset <- genWithM(mkBufferedReset);

   Clock single_clock = fclk[0];
`ifdef ZYNQ_NO_RESET
   freset[0]          = noReset;
`endif
   let single_reset   = freset[0];

   ClockGenerator7Params clockParams = defaultValue;
   // input clock 200MHz for speed grade -2, 100MHz for speed grade -1
   // fpll needs to be in the range 600MHz - 1200MHz for either input clock
   //
   // fclkin = 1e9 / mainClockPeriod
   // fpll = 1e9 = mult_f * 1e9 / mainClockPeriod
   // mult_f = mainClockPeriod
   //
   // fclkout0 = 1e9 / divide_f = 1e9 / derivedClockPeriod
   // divide_f = derivedClockPeriod
   //
   clockParams.clkfbout_mult_f       = mainClockPeriod;
   clockParams.clkfbout_phase     = 0.0;
   clockParams.clkfbout_phase     = 0.0;
   clockParams.clkin1_period      = mainClockPeriod;
   clockParams.clkout0_divide_f   = derivedClockPeriod;
   clockParams.clkout0_duty_cycle = 0.5;
   clockParams.clkout0_phase      = 0.0000;
   clockParams.clkout0_buffer     = True;
   clockParams.clkin_buffer = False;
   ClockGenerator7   clockGen <- mkClockGenerator7(clockParams, clocked_by single_clock, reset_by single_reset);
   let derived_clock = clockGen.clkout0;
   let derived_reset_unbuffered <- mkAsyncReset(2, single_reset, derived_clock);
   let derived_reset <- mkResetBUFG(clocked_by derived_clock, reset_by derived_reset_unbuffered);

   PS7LIB ps7 <- mkPS7LIB(single_clock, single_reset, clocked_by single_clock, reset_by single_reset);

   // this rule connects the fclkclk wires to the clock net via B2C
   for (Integer i = 0; i < 4; i = i + 1) begin
      ReadOnly#(Bit#(4)) fclkb;
      ReadOnly#(Bit#(4)) fclkresetnb;
      fclkb       <- mkNullCrossingWire(b2c[i].c, ps7.fclkclk);
      fclkresetnb <- mkNullCrossingWire(b2c[i].c, ps7.fclkresetn);
      rule b2c_rule1;
	 b2c[i].inputclock(fclkb[i]);
	 b2c[i].inputreset(fclkresetnb[i]);
      endrule
   end

   IDELAYCTRL idel <- mkIDELAYCTRL(2, clocked_by fclk[3], reset_by freset[0]);

    rule arb_rule;
        ps7.ddr.arb(4'b0);
    endrule

    interface ZynqPins pins;
    interface Inout  a = ps7.ddr.a;
    interface Inout  ba = ps7.ddr.ba;
    interface Inout  casb = ps7.ddr.casb;
    interface Inout  cke = ps7.ddr.cke;
    interface Inout  csb = ps7.ddr.csb;
    interface Inout  ckn = ps7.ddr.ckn;
    interface Inout  ck = ps7.ddr.ckp;
    interface Inout  dm = ps7.ddr.dm;
    interface Inout  dq = ps7.ddr.dq;
    interface Inout  dqsn = ps7.ddr.dqsn;
    interface Inout  dqs = ps7.ddr.dqsp;
    interface Inout  drstb = ps7.ddr.drstb;
    interface Inout  odt = ps7.ddr.odt;
    interface Inout  rasb = ps7.ddr.rasb;
    interface Inout  vrn = ps7.ddr.vrn;
    interface Inout  vrp = ps7.ddr.vrp;
    interface Inout  web = ps7.ddr.web;
    interface Inout  mio = ps7.mio;
    interface Pps7Ps ps = ps7.ps;
    endinterface
    interface AxiMasterCommon m_axi_gp = ps7.m_axi_gp;
    interface AxiSlaveCommon s_axi_gp = ps7.s_axi_gp;
    interface AxiSlaveHighSpeed s_axi_hp = ps7.s_axi_hp;
    interface fclkclk = fclk;
    interface fclkreset = freset;
    interface derivedClock = derived_clock;
    interface derivedReset = derived_reset;
    method Action interrupt(Bit#(1) v);
        ps7.irq.f2p({19'b0, v});
    endmethod
    interface Pps7Emioi2c       i2c = ps7.i2c;
endmodule

instance ConnectableWithTrace#(PS7, ConnectalTop#(32,64,ipins,nMasters), BscanTop);
   module mkConnectionWithTrace#(PS7 ps7, ConnectalTop#(32,64,ipins,nMasters) top, BscanTop bscan)(Empty);

      Axi3Slave#(32,32,12) ctrl <- mkAxiDmaSlave(top.slave);
      mkConnectionWithTrace(ps7.m_axi_gp[0].client, ctrl, bscan);

      module mkAxiMasterConnection#(Integer i)(Axi3Master#(32,64,6));
	 let m_axi <- mkAxiDmaMaster(top.masters[i]);
	 mkConnection(m_axi, ps7.s_axi_hp[i].axi.server);
	 return m_axi;
      endmodule
      Vector#(nMasters, Axi3Master#(32,64,6)) m_axis <- genWithM(mkAxiMasterConnection);

   endmodule
endinstance


/*
   ../scripts/importbvi.py
   -o
   AxiDmaBvi.bsv
   -P
   AxiDmaBvi
   -I
   AxiDmaBvi
   -f
   m_axi_s2mm
   -f
   s_axis_s2mm
   -f
   mm2s
   -f
   s2mm
   -f
   m_axi_mm2s
   -f
   m_axis_mm2s
   ../../out/vc709/axi_dma_0/axi_dma_0_stub.v
*/

import Clocks::*;
import DefaultValue::*;
import XilinxCells::*;
import GetPut::*;
import AxiBits::*;
import AxiStream::*;

(* always_ready, always_enabled *)
interface AxidmabviAxi;
    method Bit#(32)     dma_tstvec();
    method Action      resetn(Bit#(1) v);
endinterface
(* always_ready, always_enabled *)
interface AxidmabviM_axi_mm2s;
    method Action      aclk(Bit#(1) v);
    method Bit#(32)     araddr();
    method Bit#(2)     arburst();
    method Bit#(4)     arcache();
    method Bit#(8)     arlen();
    method Bit#(3)     arprot();
    method Action      arready(Bit#(1) v);
    method Bit#(3)     arsize();
    method Bit#(1)     arvalid();
    method Action      rdata(Bit#(32) v);
    method Action      rlast(Bit#(1) v);
    method Bit#(1)     rready();
    method Action      rresp(Bit#(2) v);
    method Action      rvalid(Bit#(1) v);
endinterface
(* always_ready, always_enabled *)
interface AxidmabviM_axi_s2mm;
    method Action      aclk(Bit#(1) v);
    method Bit#(32)     awaddr();
    method Bit#(2)     awburst();
    method Bit#(4)     awcache();
    method Bit#(8)     awlen();
    method Bit#(3)     awprot();
    method Action      awready(Bit#(1) v);
    method Bit#(3)     awsize();
    method Bit#(1)     awvalid();
    method Bit#(1)     bready();
    method Action      bresp(Bit#(2) v);
    method Action      bvalid(Bit#(1) v);
    method Bit#(32)     wdata();
    method Bit#(1)     wlast();
    method Action      wready(Bit#(1) v);
    method Bit#(4)     wstrb();
    method Bit#(1)     wvalid();
endinterface
(* always_ready, always_enabled *)
interface AxidmabviM_axi_sg;
    method Action      aclk(Bit#(1) v);
    method Bit#(32)     araddr();
    method Bit#(2)     arburst();
    method Bit#(4)     arcache();
    method Bit#(8)     arlen();
    method Bit#(3)     arprot();
    method Action      arready(Bit#(1) v);
    method Bit#(3)     arsize();
    method Bit#(1)     arvalid();
    method Bit#(32)     awaddr();
    method Bit#(2)     awburst();
    method Bit#(4)     awcache();
    method Bit#(8)     awlen();
    method Bit#(3)     awprot();
    method Action      awready(Bit#(1) v);
    method Bit#(3)     awsize();
    method Bit#(1)     awvalid();
    method Bit#(1)     bready();
    method Action      bresp(Bit#(2) v);
    method Action      bvalid(Bit#(1) v);
    method Action      rdata(Bit#(32) v);
    method Action      rlast(Bit#(1) v);
    method Bit#(1)     rready();
    method Action      rresp(Bit#(2) v);
    method Action      rvalid(Bit#(1) v);
    method Bit#(32)     wdata();
    method Bit#(1)     wlast();
    method Action      wready(Bit#(1) v);
    method Bit#(4)     wstrb();
    method Bit#(1)     wvalid();
endinterface
(* always_ready, always_enabled *)
interface AxidmabviMm2s;
    method Bit#(1)     cntrl_reset_out_n();
    method Bit#(1)     introut();
    method Bit#(1)     prmry_reset_out_n();
endinterface
(* always_ready, always_enabled *)
interface AxidmabviS2mm;
    method Bit#(1)     introut();
    method Bit#(1)     prmry_reset_out_n();
    method Bit#(1)     sts_reset_out_n();
endinterface
(* always_ready, always_enabled *)
interface AxidmabviS_axi_lite;
    method Action      aclk(Bit#(1) v);
    method Action      araddr(Bit#(10) v);
    method Bit#(1)     arready();
    method Action      arvalid(Bit#(1) v);
    method Action      awaddr(Bit#(10) v);
    method Bit#(1)     awready();
    method Action      awvalid(Bit#(1) v);
    method Action      bready(Bit#(1) v);
    method Bit#(2)     bresp();
    method Bit#(1)     bvalid();
    method Bit#(32)     rdata();
    method Action      rready(Bit#(1) v);
    method Bit#(2)     rresp();
    method Bit#(1)     rvalid();
    method Action      wdata(Bit#(32) v);
    method Bit#(1)     wready();
    method Action      wvalid(Bit#(1) v);
endinterface
(* always_ready, always_enabled *)
interface AxiDmaBvi;
    interface AxidmabviAxi     axi;
    interface AxidmabviM_axi_mm2s     m_axi_mm2s;
    interface AxidmabviM_axi_s2mm     m_axi_s2mm;
    interface AxidmabviM_axi_sg     m_axi_sg;
    interface AxiStreamMaster#(32)     m_axis_mm2s;
    interface AxiStreamMaster#(32)     m_axis_mm2s_cntrl;
    interface AxidmabviMm2s     mm2s;
    interface AxidmabviS2mm     s2mm;
    interface AxidmabviS_axi_lite     s_axi_lite;
    interface AxiStreamSlave#(32)     s_axis_s2mm_sts;
    interface AxiStreamSlave#(32)     s_axis_s2mm;
endinterface
import "BVI" axi_dma_0 =
module mkAxiDmaBvi(AxiDmaBvi);
    default_clock clk();
    default_reset rst();
    interface AxidmabviAxi     axi;
        method axi_dma_tstvec dma_tstvec();
        method resetn(axi_resetn) enable((*inhigh*) EN_axi_resetn);
    endinterface
    interface AxidmabviM_axi_mm2s     m_axi_mm2s;
        method aclk(m_axi_mm2s_aclk) enable((*inhigh*) EN_m_axi_mm2s_aclk);
        method m_axi_mm2s_araddr araddr();
        method m_axi_mm2s_arburst arburst();
        method m_axi_mm2s_arcache arcache();
        method m_axi_mm2s_arlen arlen();
        method m_axi_mm2s_arprot arprot();
        method arready(m_axi_mm2s_arready) enable((*inhigh*) EN_m_axi_mm2s_arready);
        method m_axi_mm2s_arsize arsize();
        method m_axi_mm2s_arvalid arvalid();
        method rdata(m_axi_mm2s_rdata) enable((*inhigh*) EN_m_axi_mm2s_rdata);
        method rlast(m_axi_mm2s_rlast) enable((*inhigh*) EN_m_axi_mm2s_rlast);
        method m_axi_mm2s_rready rready();
        method rresp(m_axi_mm2s_rresp) enable((*inhigh*) EN_m_axi_mm2s_rresp);
        method rvalid(m_axi_mm2s_rvalid) enable((*inhigh*) EN_m_axi_mm2s_rvalid);
    endinterface
    interface AxidmabviM_axi_s2mm     m_axi_s2mm;
        method aclk(m_axi_s2mm_aclk) enable((*inhigh*) EN_m_axi_s2mm_aclk);
        method m_axi_s2mm_awaddr awaddr();
        method m_axi_s2mm_awburst awburst();
        method m_axi_s2mm_awcache awcache();
        method m_axi_s2mm_awlen awlen();
        method m_axi_s2mm_awprot awprot();
        method awready(m_axi_s2mm_awready) enable((*inhigh*) EN_m_axi_s2mm_awready);
        method m_axi_s2mm_awsize awsize();
        method m_axi_s2mm_awvalid awvalid();
        method m_axi_s2mm_bready bready();
        method bresp(m_axi_s2mm_bresp) enable((*inhigh*) EN_m_axi_s2mm_bresp);
        method bvalid(m_axi_s2mm_bvalid) enable((*inhigh*) EN_m_axi_s2mm_bvalid);
        method m_axi_s2mm_wdata wdata();
        method m_axi_s2mm_wlast wlast();
        method wready(m_axi_s2mm_wready) enable((*inhigh*) EN_m_axi_s2mm_wready);
        method m_axi_s2mm_wstrb wstrb();
        method m_axi_s2mm_wvalid wvalid();
    endinterface
    interface AxidmabviM_axi_sg     m_axi_sg;
        method aclk(m_axi_sg_aclk) enable((*inhigh*) EN_m_axi_sg_aclk);
        method m_axi_sg_araddr araddr();
        method m_axi_sg_arburst arburst();
        method m_axi_sg_arcache arcache();
        method m_axi_sg_arlen arlen();
        method m_axi_sg_arprot arprot();
        method arready(m_axi_sg_arready) enable((*inhigh*) EN_m_axi_sg_arready);
        method m_axi_sg_arsize arsize();
        method m_axi_sg_arvalid arvalid();
        method m_axi_sg_awaddr awaddr();
        method m_axi_sg_awburst awburst();
        method m_axi_sg_awcache awcache();
        method m_axi_sg_awlen awlen();
        method m_axi_sg_awprot awprot();
        method awready(m_axi_sg_awready) enable((*inhigh*) EN_m_axi_sg_awready);
        method m_axi_sg_awsize awsize();
        method m_axi_sg_awvalid awvalid();
        method m_axi_sg_bready bready();
        method bresp(m_axi_sg_bresp) enable((*inhigh*) EN_m_axi_sg_bresp);
        method bvalid(m_axi_sg_bvalid) enable((*inhigh*) EN_m_axi_sg_bvalid);
        method rdata(m_axi_sg_rdata) enable((*inhigh*) EN_m_axi_sg_rdata);
        method rlast(m_axi_sg_rlast) enable((*inhigh*) EN_m_axi_sg_rlast);
        method m_axi_sg_rready rready();
        method rresp(m_axi_sg_rresp) enable((*inhigh*) EN_m_axi_sg_rresp);
        method rvalid(m_axi_sg_rvalid) enable((*inhigh*) EN_m_axi_sg_rvalid);
        method m_axi_sg_wdata wdata();
        method m_axi_sg_wlast wlast();
        method wready(m_axi_sg_wready) enable((*inhigh*) EN_m_axi_sg_wready);
        method m_axi_sg_wstrb wstrb();
        method m_axi_sg_wvalid wvalid();
    endinterface
    interface AxiStreamMaster     m_axis_mm2s_cntrl;
        method m_axis_mm2s_cntrl_tdata tdata();
        method m_axis_mm2s_cntrl_tkeep tkeep();
        method m_axis_mm2s_cntrl_tlast tlast();
        method tready(m_axis_mm2s_cntrl_tready) enable((*inhigh*) EN_m_axis_mm2s_cntrl_tready);
        method m_axis_mm2s_cntrl_tvalid tvalid();
   endinterface
    interface AxiStreamMaster     m_axis_mm2s;
        method m_axis_mm2s_tdata tdata();
        method m_axis_mm2s_tkeep tkeep();
        method m_axis_mm2s_tlast tlast();
        method tready(m_axis_mm2s_tready) enable((*inhigh*) EN_m_axis_mm2s_tready);
        method m_axis_mm2s_tvalid tvalid();
    endinterface
    interface AxidmabviMm2s     mm2s;
        method mm2s_cntrl_reset_out_n cntrl_reset_out_n();
        method mm2s_introut introut();
        method mm2s_prmry_reset_out_n prmry_reset_out_n();
    endinterface
    interface AxidmabviS2mm     s2mm;
        method s2mm_introut introut();
        method s2mm_prmry_reset_out_n prmry_reset_out_n();
        method s2mm_sts_reset_out_n sts_reset_out_n();
    endinterface
    interface AxidmabviS_axi_lite     s_axi_lite;
        method aclk(s_axi_lite_aclk) enable((*inhigh*) EN_s_axi_lite_aclk);
        method araddr(s_axi_lite_araddr) enable((*inhigh*) EN_s_axi_lite_araddr);
        method s_axi_lite_arready arready();
        method arvalid(s_axi_lite_arvalid) enable((*inhigh*) EN_s_axi_lite_arvalid);
        method awaddr(s_axi_lite_awaddr) enable((*inhigh*) EN_s_axi_lite_awaddr);
        method s_axi_lite_awready awready();
        method awvalid(s_axi_lite_awvalid) enable((*inhigh*) EN_s_axi_lite_awvalid);
        method bready(s_axi_lite_bready) enable((*inhigh*) EN_s_axi_lite_bready);
        method s_axi_lite_bresp bresp();
        method s_axi_lite_bvalid bvalid();
        method s_axi_lite_rdata rdata();
        method rready(s_axi_lite_rready) enable((*inhigh*) EN_s_axi_lite_rready);
        method s_axi_lite_rresp rresp();
        method s_axi_lite_rvalid rvalid();
        method wdata(s_axi_lite_wdata) enable((*inhigh*) EN_s_axi_lite_wdata);
        method s_axi_lite_wready wready();
        method wvalid(s_axi_lite_wvalid) enable((*inhigh*) EN_s_axi_lite_wvalid);
    endinterface
    interface AxiStreamSlave     s_axis_s2mm_sts;
        method tdata(s_axis_s2mm_sts_tdata) enable((*inhigh*) EN_s_axis_s2mm_sts_tdata);
        method tkeep(s_axis_s2mm_sts_tkeep) enable((*inhigh*) EN_s_axis_s2mm_sts_tkeep);
        method tlast(s_axis_s2mm_sts_tlast) enable((*inhigh*) EN_s_axis_s2mm_sts_tlast);
        method s_axis_s2mm_sts_tready tready();
        method tvalid(s_axis_s2mm_sts_tvalid) enable((*inhigh*) EN_s_axis_s2mm_sts_tvalid);
    endinterface
    interface AxiStreamSlave     s_axis_s2mm;
        method tdata(s_axis_s2mm_tdata) enable((*inhigh*) EN_s_axis_s2mm_tdata);
        method tkeep(s_axis_s2mm_tkeep) enable((*inhigh*) EN_s_axis_s2mm_tkeep);
        method tlast(s_axis_s2mm_tlast) enable((*inhigh*) EN_s_axis_s2mm_tlast);
        method s_axis_s2mm_tready tready();
        method tvalid(s_axis_s2mm_tvalid) enable((*inhigh*) EN_s_axis_s2mm_tvalid);
    endinterface
    schedule (axi.dma_tstvec, axi.resetn, m_axi_mm2s.aclk, m_axi_mm2s.araddr, m_axi_mm2s.arburst, m_axi_mm2s.arcache, m_axi_mm2s.arlen, m_axi_mm2s.arprot, m_axi_mm2s.arready, m_axi_mm2s.arsize, m_axi_mm2s.arvalid, m_axi_mm2s.rdata, m_axi_mm2s.rlast, m_axi_mm2s.rready, m_axi_mm2s.rresp, m_axi_mm2s.rvalid, m_axi_s2mm.aclk, m_axi_s2mm.awaddr, m_axi_s2mm.awburst, m_axi_s2mm.awcache, m_axi_s2mm.awlen, m_axi_s2mm.awprot, m_axi_s2mm.awready, m_axi_s2mm.awsize, m_axi_s2mm.awvalid, m_axi_s2mm.bready, m_axi_s2mm.bresp, m_axi_s2mm.bvalid, m_axi_s2mm.wdata, m_axi_s2mm.wlast, m_axi_s2mm.wready, m_axi_s2mm.wstrb, m_axi_s2mm.wvalid, m_axi_sg.aclk, m_axi_sg.araddr, m_axi_sg.arburst, m_axi_sg.arcache, m_axi_sg.arlen, m_axi_sg.arprot, m_axi_sg.arready, m_axi_sg.arsize, m_axi_sg.arvalid, m_axi_sg.awaddr, m_axi_sg.awburst, m_axi_sg.awcache, m_axi_sg.awlen, m_axi_sg.awprot, m_axi_sg.awready, m_axi_sg.awsize, m_axi_sg.awvalid, m_axi_sg.bready, m_axi_sg.bresp, m_axi_sg.bvalid, m_axi_sg.rdata, m_axi_sg.rlast, m_axi_sg.rready, m_axi_sg.rresp, m_axi_sg.rvalid, m_axi_sg.wdata, m_axi_sg.wlast, m_axi_sg.wready, m_axi_sg.wstrb, m_axi_sg.wvalid, m_axis_mm2s_cntrl.tdata, m_axis_mm2s_cntrl.tkeep, m_axis_mm2s_cntrl.tlast, m_axis_mm2s_cntrl.tready, m_axis_mm2s_cntrl.tvalid, m_axis_mm2s.tdata, m_axis_mm2s.tkeep, m_axis_mm2s.tlast, m_axis_mm2s.tready, m_axis_mm2s.tvalid, mm2s.cntrl_reset_out_n, mm2s.introut, mm2s.prmry_reset_out_n, s2mm.introut, s2mm.prmry_reset_out_n, s2mm.sts_reset_out_n, s_axi_lite.aclk, s_axi_lite.araddr, s_axi_lite.arready, s_axi_lite.arvalid, s_axi_lite.awaddr, s_axi_lite.awready, s_axi_lite.awvalid, s_axi_lite.bready, s_axi_lite.bresp, s_axi_lite.bvalid, s_axi_lite.rdata, s_axi_lite.rready, s_axi_lite.rresp, s_axi_lite.rvalid, s_axi_lite.wdata, s_axi_lite.wready, s_axi_lite.wvalid, s_axis_s2mm_sts.tdata, s_axis_s2mm_sts.tkeep, s_axis_s2mm_sts.tlast, s_axis_s2mm_sts.tready, s_axis_s2mm_sts.tvalid, s_axis_s2mm.tdata, s_axis_s2mm.tkeep, s_axis_s2mm.tlast, s_axis_s2mm.tready, s_axis_s2mm.tvalid) CF (axi.dma_tstvec, axi.resetn, m_axi_mm2s.aclk, m_axi_mm2s.araddr, m_axi_mm2s.arburst, m_axi_mm2s.arcache, m_axi_mm2s.arlen, m_axi_mm2s.arprot, m_axi_mm2s.arready, m_axi_mm2s.arsize, m_axi_mm2s.arvalid, m_axi_mm2s.rdata, m_axi_mm2s.rlast, m_axi_mm2s.rready, m_axi_mm2s.rresp, m_axi_mm2s.rvalid, m_axi_s2mm.aclk, m_axi_s2mm.awaddr, m_axi_s2mm.awburst, m_axi_s2mm.awcache, m_axi_s2mm.awlen, m_axi_s2mm.awprot, m_axi_s2mm.awready, m_axi_s2mm.awsize, m_axi_s2mm.awvalid, m_axi_s2mm.bready, m_axi_s2mm.bresp, m_axi_s2mm.bvalid, m_axi_s2mm.wdata, m_axi_s2mm.wlast, m_axi_s2mm.wready, m_axi_s2mm.wstrb, m_axi_s2mm.wvalid, m_axi_sg.aclk, m_axi_sg.araddr, m_axi_sg.arburst, m_axi_sg.arcache, m_axi_sg.arlen, m_axi_sg.arprot, m_axi_sg.arready, m_axi_sg.arsize, m_axi_sg.arvalid, m_axi_sg.awaddr, m_axi_sg.awburst, m_axi_sg.awcache, m_axi_sg.awlen, m_axi_sg.awprot, m_axi_sg.awready, m_axi_sg.awsize, m_axi_sg.awvalid, m_axi_sg.bready, m_axi_sg.bresp, m_axi_sg.bvalid, m_axi_sg.rdata, m_axi_sg.rlast, m_axi_sg.rready, m_axi_sg.rresp, m_axi_sg.rvalid, m_axi_sg.wdata, m_axi_sg.wlast, m_axi_sg.wready, m_axi_sg.wstrb, m_axi_sg.wvalid, m_axis_mm2s_cntrl.tdata, m_axis_mm2s_cntrl.tkeep, m_axis_mm2s_cntrl.tlast, m_axis_mm2s_cntrl.tready, m_axis_mm2s_cntrl.tvalid, m_axis_mm2s.tdata, m_axis_mm2s.tkeep, m_axis_mm2s.tlast, m_axis_mm2s.tready, m_axis_mm2s.tvalid, mm2s.cntrl_reset_out_n, mm2s.introut, mm2s.prmry_reset_out_n, s2mm.introut, s2mm.prmry_reset_out_n, s2mm.sts_reset_out_n, s_axi_lite.aclk, s_axi_lite.araddr, s_axi_lite.arready, s_axi_lite.arvalid, s_axi_lite.awaddr, s_axi_lite.awready, s_axi_lite.awvalid, s_axi_lite.bready, s_axi_lite.bresp, s_axi_lite.bvalid, s_axi_lite.rdata, s_axi_lite.rready, s_axi_lite.rresp, s_axi_lite.rvalid, s_axi_lite.wdata, s_axi_lite.wready, s_axi_lite.wvalid, s_axis_s2mm_sts.tdata, s_axis_s2mm_sts.tkeep, s_axis_s2mm_sts.tlast, s_axis_s2mm_sts.tready, s_axis_s2mm_sts.tvalid, s_axis_s2mm.tdata, s_axis_s2mm.tkeep, s_axis_s2mm.tlast, s_axis_s2mm.tready, s_axis_s2mm.tvalid);
endmodule

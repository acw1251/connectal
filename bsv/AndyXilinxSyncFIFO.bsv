// AndyXilinxSyncFIFO.bsv
import GetPut::*;
import Vector::*;

// module sync_fifo_w32_d16(wr_clk, rd_clk, din, wr_en, rd_en, dout, full, empty)
//   input wr_clk;
//   input rd_clk;
//   input [31:0]din;
//   input wr_en;
//   input rd_en;
//   output [31:0]dout;
//   output full;
//   output empty;
// endmodule

interface SyncFIFO_BVI#(numeric type dataWidth);
    // unguarded write interface
    method Action enq(Bit#(dataWidth) data);
    method Bool full;
    // unguarded read interface
    method Action deq();
    method Bit#(dataWidth) first;
    method Bool empty;
endinterface
import "BVI" sync_fifo_w32_d16 =
module vMkSyncFIFO_w32_d16#(Clock wr_clk, Clock rd_clk)(SyncFIFO_BVI#(32));
    default_clock no_clock;
    default_reset no_reset;

    input_clock wr_clk(wr_clk, (* unused *) DUMMY_wr_clk_gate) = wr_clk;
    input_clock rd_clk(rd_clk, (* unused *) DUMMY_wr_clk_gate) = rd_clk;

    method enq(din) enable(wr_en) clocked_by(wr_clk);
    method full full() clocked_by(wr_clk);

    method deq() enable(rd_en) clocked_by(rd_clk);
    method dout first() clocked_by(rd_clk);
    method empty empty() clocked_by(rd_clk);
endmodule

interface AndySyncFIFO#(type t);
    interface Put#(t) write;
    interface Get#(t) read;
endinterface

(* no_default_clock, no_default_reset *)
module mkAndySyncFIFO#(Clock wr_clk, Clock rd_clk)(AndySyncFIFO#(t))
        provisos (Bits#(t, tSz),
                  NumAlias#(nChunks, TDiv#(tSz,32)),
                  Add#(a__, tSz, TMul#(nChunks, 32)));
    Vector#(nChunks, SyncFIFO_BVI#(32)) fifos <- replicateM(vMkSyncFIFO_w32_d16(wr_clk, rd_clk));

    // Helper functions
    function Bool readyForRead( SyncFIFO_BVI#(32) fifo ) = !fifo.empty;
    function Bool readyForWrite( SyncFIFO_BVI#(32) fifo ) = !fifo.full;
    function ActionValue#(Bit#(32)) readFifo( SyncFIFO_BVI#(32) fifo ) = (actionvalue fifo.deq; return fifo.first; endactionvalue);
    function Action writeFifo( SyncFIFO_BVI#(32) fifo, Bit#(32) data ) = (action fifo.enq(data); endaction);

    interface Put write;
        method Action put(t x) if (all(readyForWrite, fifos));
            Vector#(nChunks, Bit#(32)) writeData = unpack(zeroExtend(pack(x)));
            zipWithM_(writeFifo, fifos, writeData);
        endmethod
    endinterface
    interface Get read;
        method ActionValue#(t) get() if (all(readyForRead, fifos));
            Vector#(nChunks, Bit#(32)) readData <- mapM(readFifo, fifos);
            t returnValue = unpack(truncate(pack(readData)));
            return returnValue;
        endmethod
    endinterface
endmodule


import GetPut::*;
import MemTypes::*;
import Vector::*;

typeclass AddImplicitCondition#(type t);
    function t addImplicitCondition(Bool b, t x);
endtypeclass

instance AddImplicitCondition#(Vector#(n, t)) provisos (AddImplicitCondition#(t));
    function Vector#(n, t) addImplicitCondition(Bool b, Vector#(n, t) x);
        return map(addImplicitCondition(b), x);
    endfunction
endinstance

instance AddImplicitCondition#(ActionValue#(t));
    function ActionValue#(t) addImplicitCondition(Bool b, ActionValue#(t) x);
        return (actionvalue
                when(b, noAction);
                let ret_val <- x;
                return ret_val;
            endactionvalue);
    endfunction
endinstance

instance AddImplicitCondition#(function out_t f(in_t x)) provisos (AddImplicitCondition#(out_t));
    function (function out_t f(in_t x)) addImplicitCondition(Bool b, function out_t func(in_t x));
        function out_t outFunc(in_t x);
            return addImplicitCondition(b, func(x));
        endfunction
        return outFunc;
    endfunction
endinstance

instance AddImplicitCondition#(Get#(t));
    function Get#(t) addImplicitCondition(Bool b, Get#(t) x);
        return (interface Get;
                    method get = addImplicitCondition(b, x.get);
                endinterface);
    endfunction
endinstance

instance AddImplicitCondition#(Put#(t));
    function Put#(t) addImplicitCondition(Bool b, Put#(t) x);
        return (interface Put;
                    method put = addImplicitCondition(b, x.put);
                endinterface);
    endfunction
endinstance

instance AddImplicitCondition#(PhysMemReadClient#(asz, dsz));
    function PhysMemReadClient#(asz, dsz) addImplicitCondition(Bool b, PhysMemReadClient#(asz, dsz) x);
        return (interface PhysMemReadClient;
                    interface readReq = addImplicitCondition(b, x.readReq);
                    interface readData = addImplicitCondition(b, x.readData);
                endinterface);
    endfunction
endinstance

instance AddImplicitCondition#(PhysMemWriteClient#(asz, dsz));
    function PhysMemWriteClient#(asz, dsz) addImplicitCondition(Bool b, PhysMemWriteClient#(asz, dsz) x);
        return (interface PhysMemWriteClient;
                    interface writeReq = addImplicitCondition(b, x.writeReq);
                    interface writeData = addImplicitCondition(b, x.writeData);
                    interface writeDone = addImplicitCondition(b, x.writeDone);
                endinterface);
    endfunction
endinstance

instance AddImplicitCondition#(PhysMemReadServer#(asz, dsz));
    function PhysMemReadServer#(asz, dsz) addImplicitCondition(Bool b, PhysMemReadServer#(asz, dsz) x);
        return (interface PhysMemReadServer;
                    interface readReq = addImplicitCondition(b, x.readReq);
                    interface readData = addImplicitCondition(b, x.readData);
                endinterface);
    endfunction
endinstance

instance AddImplicitCondition#(PhysMemWriteServer#(asz, dsz));
    function PhysMemWriteServer#(asz, dsz) addImplicitCondition(Bool b, PhysMemWriteServer#(asz, dsz) x);
        return (interface PhysMemWriteServer;
                    interface writeReq = addImplicitCondition(b, x.writeReq);
                    interface writeData = addImplicitCondition(b, x.writeData);
                    interface writeDone = addImplicitCondition(b, x.writeDone);
                endinterface);
    endfunction
endinstance

instance AddImplicitCondition#(PhysMemSlave#(asz, dsz));
    function PhysMemSlave#(asz, dsz) addImplicitCondition(Bool b, PhysMemSlave#(asz, dsz) x);
        return (interface PhysMemSlave;
                    interface read_server = addImplicitCondition(b, x.read_server);
                    interface write_server = addImplicitCondition(b, x.write_server);
                endinterface);
    endfunction
endinstance

instance AddImplicitCondition#(PhysMemMaster#(asz, dsz));
    function PhysMemMaster#(asz, dsz) addImplicitCondition(Bool b, PhysMemMaster#(asz, dsz) x);
        return (interface PhysMemMaster;
                    interface read_client = addImplicitCondition(b, x.read_client);
                    interface write_client = addImplicitCondition(b, x.write_client);
                endinterface);
    endfunction
endinstance

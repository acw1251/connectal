CONNECTALDIR?=../..

S2H_INTERFACES = \
	FastEchoRequest:FastEcho.request \

H2S_INTERFACES = \
	FastEcho:FastEchoIndication \

BSVFILES = FastEcho.bsv
CPPFILES = testfastecho.cpp

# This matches the frequency of the original design where the bug was found
CONNECTALFLAGS += --mainclockperiod=32

CONNECTALFLAGS += -D GET_PUT_WITH_CLOCKS_USE_XILINX_FIFO
CONNECTALFLAGS += --xci=$(IPDIR)/$(BOARD)/dual_clock_axis_fifo_32x8/dual_clock_axis_fifo_32x8.xci

# CONNECTALFLAGS += -D GET_PUT_WITH_CLOCKS_USE_ANDY_FIFO
# CONNECTALFLAGS += --xci=$(IPDIR)/$(BOARD)/sync_fifo_w32_d16/sync_fifo_w32_d16.xci

include $(CONNECTALDIR)/Makefile.connectal


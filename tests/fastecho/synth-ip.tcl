source "board.tcl" 
source "$connectaldir/../fpgamake/tcl/ipcore.tcl"

if {[version -short] >= "2016.1"} {
    set fifo_generator_version 13.1
} else {
    set fifo_generator_version 13.0
}

fpgamake_ipcore fifo_generator $fifo_generator_version dual_clock_axis_fifo_32x8 [list \
                           config.interface_type {axi_stream} \
                           config.clock_type_axi {independent_clock} \
                           config.tdata_num_bytes {4} \
                           config.tuser_width {0} \
                           config.enable_tlast {true} \
                           config.has_tkeep {true} \
                           config.fifo_application_type_axis {data_fifo} \
                           config.reset_type {asynchronous_reset} \
                           ]

fpgamake_ipcore fifo_generator $fifo_generator_version sync_fifo_w32_d16 [list \
                           CONFIG.Fifo_Implementation {Independent_Clocks_Distributed_RAM} \
                           CONFIG.Performance_Options {First_Word_Fall_Through} \
                           CONFIG.Input_Data_Width {32} \
                           CONFIG.Input_Depth {16} \
                           CONFIG.Output_Data_Width {32} \
                           CONFIG.Output_Depth {16} \
                           CONFIG.Reset_Pin {false} \
                           ]


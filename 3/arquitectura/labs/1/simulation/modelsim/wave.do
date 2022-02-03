onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /processor_tb/CLOCK_50
add wave -noupdate /processor_tb/reset
add wave -noupdate /processor_tb/DM_writeEnable
add wave -noupdate -radix hexadecimal /processor_tb/DM_writeData
add wave -noupdate -radix hexadecimal /processor_tb/DM_addr
add wave -noupdate -radix hexadecimal /processor_tb/dump
add wave -noupdate -divider Decode
add wave -noupdate /processor_tb/dut/dp/DECODE/registers/ra1
add wave -noupdate /processor_tb/dut/dp/DECODE/registers/ra2
add wave -noupdate -radix decimal /processor_tb/dut/dp/DECODE/registers/rd1
add wave -noupdate -radix decimal /processor_tb/dut/dp/DECODE/registers/rd2
add wave -noupdate -radix hexadecimal -childformat {{{/processor_tb/dut/instrMem/q[31]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[30]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[29]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[28]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[27]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[26]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[25]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[24]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[23]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[22]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[21]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[20]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[19]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[18]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[17]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[16]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[15]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[14]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[13]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[12]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[11]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[10]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[9]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[8]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[7]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[6]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[5]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[4]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[3]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[2]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[1]} -radix hexadecimal} {{/processor_tb/dut/instrMem/q[0]} -radix hexadecimal}} -subitemconfig {{/processor_tb/dut/instrMem/q[31]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[30]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[29]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[28]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[27]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[26]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[25]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[24]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[23]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[22]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[21]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[20]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[19]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[18]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[17]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[16]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[15]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[14]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[13]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[12]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[11]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[10]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[9]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[8]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[7]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[6]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[5]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[4]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[3]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[2]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[1]} {-height 16 -radix hexadecimal} {/processor_tb/dut/instrMem/q[0]} {-height 16 -radix hexadecimal}} /processor_tb/dut/instrMem/q
add wave -noupdate -divider Execute
add wave -noupdate -radix decimal /processor_tb/dut/dp/EXECUTE/PCBranch_E
add wave -noupdate -radix decimal /processor_tb/dut/dp/EXECUTE/writeData_E
add wave -noupdate -radix decimal /processor_tb/dut/dp/EXECUTE/aluResult_E
add wave -noupdate /processor_tb/dut/dp/EXECUTE/alu/write_flags
add wave -noupdate /processor_tb/dut/dp/EXECUTE/alu/CPSR_flags
add wave -noupdate -divider Memory
add wave -noupdate /processor_tb/dut/dp/MEMORY/Branch_M
add wave -noupdate /processor_tb/dut/dp/MEMORY/zero_M
add wave -noupdate /processor_tb/dut/dp/MEMORY/CPSR_flags
add wave -noupdate /processor_tb/dut/dp/MEMORY/write_flags
add wave -noupdate /processor_tb/dut/dp/MEMORY/condBranch
add wave -noupdate /processor_tb/dut/dp/MEMORY/qIF_ID
add wave -noupdate /processor_tb/dut/dp/MEMORY/PCSrc_M
add wave -noupdate -divider WriteBack
add wave -noupdate -radix hexadecimal /processor_tb/dut/dp/WRITEBACK/writeData3_W
add wave -noupdate -radix hexadecimal /processor_tb/dut/dp/WRITEBACK/aluResult_W
add wave -noupdate -radix hexadecimal /processor_tb/dut/dp/WRITEBACK/DM_readData_W
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 5} {406 ps} 0} {{Cursor 2} {451 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 343
configure wave -valuecolwidth 186
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ps
update
WaveRestoreZoom {0 ps} {722 ps}

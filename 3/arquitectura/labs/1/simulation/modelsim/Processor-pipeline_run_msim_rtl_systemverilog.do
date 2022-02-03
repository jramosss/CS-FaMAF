transcript on
if {[file exists rtl_work]} {
	vdel -lib rtl_work -all
}
vlib rtl_work
vmap work rtl_work

vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/writeback.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/flopr.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/sl2.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/fetch.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/execute.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/signext.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/regfile.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/decode.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/processor_arm.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/datapath.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/controller.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/mux2.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/memory.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/alu.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/aludec.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/maindec.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/imem.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/adder.sv}
vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/bcondcheck.sv}
vcom -93 -work work {/home/usuario/Escritorio/Laboratorio1-Arq/dmem.vhd}

vlog -sv -work work +incdir+/home/usuario/Escritorio/Laboratorio1-Arq {/home/usuario/Escritorio/Laboratorio1-Arq/processor_tb.sv}

vsim -t 1ps -L altera_ver -L lpm_ver -L sgate_ver -L altera_mf_ver -L altera_lnsim_ver -L cycloneive_ver -L rtl_work -L work -voptargs="+acc"  processor_tb

add wave *
view structure
view signals
run -all

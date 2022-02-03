module execute #(parameter N = 64)
					(input logic AluSrc,
					input logic [3:0] AluControl,
					input logic [N-1:0] PC_E, signImm_E, readData1_E, readData2_E,
					output logic [N-1:0] PCBranch_E, aluResult_E, writeData_E,
					output logic zero_E, write_flags,
					output logic [3:0] CPSR_flags_E);
					
	logic [N-1:0] y_mux, y_sl2;				
	
	mux2 alumux (.d0(readData2_E), .d1(signImm_E), .s(AluSrc), .y(y_mux));
	
	alu alu (.a(readData1_E), .b(y_mux), .ALUControl(AluControl), .result(aluResult_E), .write_flags(write_flags), .zero(zero_E), .CPSR_flags(CPSR_flags_E));
	
	sl2 sl2 (.a(signImm_E), .y(y_sl2));
	
	adder add (.a(PC_E), .b(y_sl2), .y(PCBranch_E));
	
	assign writeData_E = readData2_E;
	
endmodule 
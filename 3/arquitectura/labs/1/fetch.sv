module fetch #(parameter N = 64)
				(input logic PCSrc_F, clk, reset, 
				 input logic [N-1:0] PCBranch_F,
				 output logic [N-1:0] imem_addr_F);
				 
	logic [N-1:0] addResult, muxResult;			 
				 
	mux2 mux2 (.d0(addResult), .d1(PCBranch_F), .s(PCSrc_F), .y(muxResult));
	
	flopr fflop (.clk(clk), .reset(reset), .d(muxResult), .q(imem_addr_F));
				 
	adder adder (.a(imem_addr_F), .b(64'b100), .y(addResult));
	
endmodule
				 
			
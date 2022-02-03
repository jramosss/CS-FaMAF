module flopr #(parameter N=64)
				 (input logic clk,reset,
				 input logic [N-1:0] d,
				 output logic [N-1:0] q);
	always_ff @(posedge clk)
		if (reset) q <= 64'h0; 
		else q <= d;
				 
endmodule 
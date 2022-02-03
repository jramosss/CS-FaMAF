// SHIFT LEFT 2

module sl2 #(parameter N = 64)
				(input logic [N-1:0] a,					
				output logic [N-1:0] y);

	assign y[0] = 1'b0;
	assign y[1] = 1'b0;
	assign y[63:2] = a[61:0];
	
endmodule
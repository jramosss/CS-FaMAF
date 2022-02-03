module imem #(parameter N = 32)
				(input logic [6:0] addr,
				output logic [N-1:0]q);
				
	logic [N-1:0] ROM [0:55] = '{default:32'b0};
	
	
	
	always_comb begin
			/*ROM [0:18] = '{32'hf8000000, 32'hf8008001,32'hf8010002, 32'hf8018003,
			32'hf8020004, 32'hf8028005,32'hf8030006, 32'hf8400007,
			32'hf8408008, 32'hf8410009,32'hf841800a, 32'hf842000b,
			32'hf842800c, 32'hf843000d,32'hcb0e01ce, 32'hb400004e,
			32'hcb01000f, 32'h8b01000f,32'hf803800f}; */
ROM [0:55] ='{32'hf8000001,
32'hf8008002,
32'hf8000203,
32'h8a1602b4,
32'h8b1f03ff,
32'h8b1f03ff,
32'h8b1f03ff,
32'hf8018014,
32'haa1602b4,
32'h8b1f03ff,
32'h8b1f03ff,
32'h8b1f03ff,
32'hf8020014,
32'hcb030294,
32'h8b1f03ff,
32'h8b1f03ff,
32'h8b1f03ff,
32'hf8028014,
32'hf840000d,
32'h8b1f03ff,
32'h8b1f03ff,
32'h8b1f03ff,
32'hf803000d,
32'hf803801f,
32'hb40000a0,
32'h8b1f03ff,
32'h8b1f03ff,
32'h8b1f03ff,
32'hf8040015,
32'h8b1e03c1,
32'h8b0503e2,
32'h8b0a03e3,
32'h8b1f03ff,
32'h8b1f03ff,
32'h8b1f03ff,
32'h8b0c0021,
32'hb400009f,
32'h8b1f03ff,
32'h8b1f03ff,
32'h8b1f03ff,
32'heb050042,
32'h540000c0,
32'h8b1f03ff,
32'h8b1f03ff,
32'h8b1f03ff,
32'hf8000022,
32'h8b080021,
32'heb050063,
32'h540000e1,
32'h8b1f03ff,
32'h8b1f03ff,
32'h8b1f03ff,
32'hf8000023,
32'h8b080021,
32'hb4ffff3f,
32'hb400001f};








			q = ROM[addr];
		
	end
				
endmodule
		
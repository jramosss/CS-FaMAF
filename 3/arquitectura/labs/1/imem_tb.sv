module imem_tb();

  logic [5:0] addr;
  logic [31:0] q;
  logic [7:0] dir [0:24] = {8'h00,8'h01,8'h02,8'h03,8'h04,8'h05,8'h06,8'h07,8'h08,
                             8'h09,8'h0a,8'h0b,8'h0c, 8'h0d, 8'h0e, 8'h0f, 8'h10, 8'h11,
									  8'h12,8'h13,8'h14,8'h15,8'h16,8'h17,8'h18};
									  
  	logic [31:0] ROM [0:18] = '{32'hf8000000, 32'hf8008001,32'hf8010002, 32'hf8018003,
	                            32'hf8020004, 32'hf8028005,32'hf8030006, 32'hf8400007,
										 32'hf8408008, 32'hf8410009,32'hf841800a, 32'hf842000b,
										 32'hf842800c, 32'hf843000d,32'hcb0e01ce, 32'hb400004e,
										 32'hcb01000f, 32'h8b01000f,32'hf803800f};
									  
	imem dut(addr, q);
	
	initial begin
	
		for(int i=0; i <= 24; i++) begin		
			addr = dir[i];
			#1;
			if(i < 19)
				assert(ROM[addr] === q) $display("200 OK");
			else
				assert(q === 0) $display(" q = 0, 200 OK");
		end
	end
	
endmodule
  
  
  
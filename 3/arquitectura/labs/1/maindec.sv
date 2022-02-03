module maindec(input logic [10:0] Op,
					output logic Reg2Loc, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, 
					output logic [1:0] ALUOp);
	
	logic [0:8] outp;
	
	always_comb begin	
		casez(Op)
			// LDUR
			11'b111_1100_0010: outp = 9'b0_1111_0000;
			// STUR
			11'b111_1100_0000: outp = 9'b1_1000_1000;
			// CBZ
			11'b101_1010_0???: outp = 9'b1_0000_0101;
			// B.cond
			11'b010_1010_0???: outp = 9'b1_0000_0101;
			// R-Types ----
			// ADD
			11'b100_0101_1000: outp = 9'b0_0010_0010;
			// SUB
			11'b110_0101_1000: outp = 9'b0_0010_0010;
			// AND
			11'b100_0101_0000: outp = 9'b0_0010_0010;
			// ORR
			11'b101_0101_0000: outp = 9'b0_0010_0010;
			// ADDS
			11'b101_0101_1000: outp = 9'b0_0010_0010;
			// SUBS
			11'b111_0101_1000: outp = 9'b0_0010_0010;
			default: outp = 9'b0;
		endcase 
	end 
	
	assign Reg2Loc = outp[0];
	assign ALUSrc = outp[1];
	assign MemtoReg = outp[2]; 
	assign RegWrite = outp[3];
	assign MemRead = outp[4];
	assign MemWrite = outp[5];
	assign Branch = outp[6];
	assign ALUOp = outp[7:8];
	
endmodule 
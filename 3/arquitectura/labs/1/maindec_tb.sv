module maindec_tb();
   logic [10:0] Op;
	logic Reg2Loc, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch;
	logic [1:0] ALUOp;
	
	maindec dut(Op,Reg2Loc, ALUSrc, MemtoReg, RegWrite, MemRead, MemWrite, Branch, ALUOp);
	
	initial begin
		//LDUR
		Op = 11'b111_1100_0010;
		#1;
		assert(!Reg2Loc && ALUSrc && MemtoReg && RegWrite && MemRead && !MemWrite && !Branch && ALUOp === 2'b00) 
		$display("LDUR OK");
		
		//STUR
		Op = 11'b111_1100_0000;
		#1;
		assert(Reg2Loc && ALUSrc && !MemtoReg && !RegWrite && !MemRead && MemWrite && !Branch && ALUOp === 2'b00) 
		$display("STUR OK");
		
		//CBZ
		Op = 11'b101_1010_0???;
		#1;
		assert(Reg2Loc && !ALUSrc && !MemtoReg && !RegWrite && !MemRead && !MemWrite && Branch && ALUOp === 2'b01) 
		$display("CBZ OK");
		
		
		//ADD
		Op = 11'b100_0101_1000;
		#1;
		assert(!Reg2Loc && !ALUSrc && !MemtoReg && RegWrite && !MemRead && !MemWrite && !Branch && ALUOp === 2'b10) 
		$display("ADD OK");
		
		//SUB
		Op = 11'b110_0101_1000;
		#1;
		assert(!Reg2Loc && !ALUSrc && !MemtoReg && RegWrite && !MemRead && !MemWrite && !Branch && ALUOp === 2'b10) 
		$display("SUB OK");
		
		//AND
		Op = 11'b100_0101_0000;
		#1;
		assert(!Reg2Loc && !ALUSrc && !MemtoReg && RegWrite && !MemRead && !MemWrite && !Branch && ALUOp === 2'b10) 
		$display("AND OK");
		
		//ORR
		Op = 11'b101_0101_0000;
		#1;
		assert(!Reg2Loc && !ALUSrc && !MemtoReg && RegWrite && !MemRead && !MemWrite && !Branch && ALUOp === 2'b10) 
		$display("ORR OK");
		
		//INVALID 
		Op = 11'b011_1111_1110;
		#1;
		assert(ALUOp === 9'b0)
		$display("INVALID OK");
		
	end
	
endmodule


module bcondcheck(input logic [3:0] CPSR_flags,                  					
						input logic [4:0] qIF_ID,					
						output logic condBranch);

logic Z,N,C,V;
assign Z = CPSR_flags[0];
assign N = CPSR_flags[1];
assign C = CPSR_flags[2];
assign V = CPSR_flags[3];

always_comb
	case(qIF_ID)
		5'b00000: condBranch =  Z;								//B.EQ
		5'b00001: condBranch = ~Z;								//B.NE
		//SIGNED
		5'b01010: condBranch = (N===V);						//B.GE
		5'b01011: condBranch = (N !== V);						//B.LT
		5'b01100: condBranch = (Z===0) & (N===V);			//B.GT
		5'b01101: condBranch = ~((Z===0) & (N===V));		//B.LE
		//UNSIGNED
		5'b01000: condBranch = (Z===0) & C;					//B.HI
		5'b01001: condBranch = ~((Z===0) & C); 				//B.LS
		5'b00010: condBranch =  C;								//B.HS
		5'b00011: condBranch = ~C;								//B.LO
		//BOTH
		5'b00100: condBranch =  N;								//B.MI
		5'b00101: condBranch = ~N;								//B.PL
		5'b00110: condBranch =  V;								//B.VS
		5'b00111: condBranch = ~V;						 		//B.VC
		default: condBranch = 1'b0;
	endcase
		
						
endmodule 
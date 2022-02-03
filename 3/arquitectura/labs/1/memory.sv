// Etapa: MEMORY

module memory     (input logic Branch_M, zero_M, write_flags,
						 input logic [3:0] CPSR_flags,
                     input logic [4:0] qIF_ID,
                     output logic PCSrc_M);


    logic condBranch;
    bcondcheck branch (.CPSR_flags(CPSR_flags), .qIF_ID(qIF_ID), .condBranch(condBranch));
    assign PCSrc_M = (write_flags) ? condBranch : (Branch_M & zero_M);


 endmodule
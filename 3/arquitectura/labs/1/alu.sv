module alu (input logic [63:0] a, b, 
                input logic [3:0] ALUControl,
                output logic [63:0] result,
                output logic zero, write_flags,
                output logic [3:0] CPSR_flags);
    
    logic V,N, Z, C;
    always_comb begin
     
        case(ALUControl)
                4'b0000: {C,result} = a & b;
                4'b0001: {C,result} = a | b;
                4'b0010: {C,result} = a + b;   
                4'b0110: {C,result} = a - b; 
                4'b0111: {C,result} = b; 
                4'b1100: {C,result} = ~(a | b);
                4'b1010: {C,result} = a + b; //ADDS
                4'b1110: {C,result} = a + (~b + 64'b1); //SUBS
                4'b1001: {C,result} = a; //B.cond
            default: {C,result} = 64'b0; 
        endcase
          
          write_flags = ((ALUControl === 4'b1010) || (ALUControl === 4'b1110));
          
          zero = (result === 0); 
          V = (a[63] ^ b[63]) ? '0 : (result[63] ^ a[63]);
          N = (result < 0);
          Z = zero;
          
          /*Se escriben alreves, el bit 3 es el primero que aparece en un string*/
          if (write_flags) begin
                CPSR_flags[0] = V;
                CPSR_flags[1] = C;
                CPSR_flags[2] = N;
                CPSR_flags[3] = Z;
          end
          else begin
                CPSR_flags = 4'b0;
            end
              
    end
endmodule 
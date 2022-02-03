module alu_tb();
  logic [63:0] a, b, result;
  logic [3:0] control;
  logic zero,write_flags;
  logic [3:0] CPSR_FLAGS;
  
  alu dut(a,b,control,result,zero,write_flags,CPSR_FLAGS);
  
  initial begin
  
     a =64'b0001;
	  b =64'b0001;
	  control = 4'b0000;
	  #2;
	  $display("AND POSITIVE && POSITIVE -- resultado = %b , zero = %b", result, zero);
	  
	  a =64'b0001;
	  b =-64'b0001;
	  control = 4'b0000;
	  #2;
	  $display("AND POSIITIVE && NEGATIVE-- resultado = %b , zero = %b", result, zero);
	  
	  a =64'b0001;
	  b =64'b0001;
	  control = 4'b0000;
	  #2;
	  $display("AND NEGATIVE && NEGATIVE-- resultado = %b , zero = %b", result, zero);
	  
	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b0001;
	  #2;
	  $display("OR POSITIVE || POSITIVE -- resultado = %b , zero = %b", result, zero);
	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b0001;
	  #2;
	  $display("OR POSITIVE || NEGATIVE -- resultado = %b , zero = %b", result, zero);
	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b0001;
	  #2;
	  $display("OR NEGATIVE || NEGATIVE -- resultado = %b , zero = %b", result, zero);
	  
	  
     a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b0010;
	  #2;
	  $display("ADD POSITIVE ++ POSITIVE-- resultado = %b , zero = %b", result, zero);
	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b0010;
	  #2;
	  $display("ADD POSITIVE ++ NEGATIVE -- resultado = %b , zero = %b", result, zero);
	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b0010;
	  #2;
	  $display("ADD NEGATIVE ++ NEGATIVE -- resultado = %b , zero = %b", result, zero);
	  
	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b0110;
	  #2;
	  $display("SUB POSITIVE -- POSTIVE -- resultado = %b , zero = %b", result, zero);
	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b0110;
	  #2;
	  $display("SUB POSITIVE -- NEGATIVE -- resultado = %b , zero = %b", result, zero);
	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b0110;
	  #2;
	  $display("SUB NEGATIVE -- NEGATIVE -- resultado = %b , zero = %b", result, zero);

	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b0111;
	  #2;
	  $display("PASS b POSITIVE POSITIVE-- resultado = %b , zero = %b", result, zero);
	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b0111;
	  #2;
	  $display("PASS b POSITIVE NEGATIVE-- resultado = %b , zero = %b", result, zero);

	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b0111;
	  #2;
	  $display("PASS b NEGATIVE NEGATIVE-- resultado = %b , zero = %b", result, zero);


     a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b1100;
	  #2;
	  $display("NOR POSITIVE POSITIVE-- resultado = %b , zero = %b", result, zero);
	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b1100;
	  #2;
	  $display("NOR POSITIVE NEGATIVE-- resultado = %b , zero = %b", result, zero);
	  
	  a = 64'b0001;
	  b = 64'b0001;
	  control = 4'b1100;
	  #2;
	  $display("NOR NEGATIVE NEGATIVE-- resultado = %b , zero = %b", result, zero);
	  
	  a = 64'b1111;
	  b = 64'b1111;
	  control = 4'b0010;
	  #2;
	  $display("ADD CARRY -- resultado = %b , zero = %b", result, zero);

	  a = '1;
	  b = '1;
	  control = 4'b0010;
	  #2
	  $display("ADD 64,64 -- resultado = %b , zero = %b", result, zero);
	  
	  
	  $display("==========================================FLAGS SETTING OPERATIONS==========================================");
	  
	  a = '1;
	  b = '0;
	  control = 4'b1110;
	  #2
	  $display("SUBS: todos 1 menos todos 0: result = %b, zero,negative,carry,overflow = %b",result,CPSR_FLAGS);
	  
	  a = '1;
	  b = '0;
	  control = 4'b1010;
	  #2
	  $display("ADDS: todos 1 menos todos 0: result = %b, zero,negative,carry,overflow = %b",result,CPSR_FLAGS);
	  
	  a = '0;
	  b = '1;
	  control = 4'b1110;
	  #2
	  $display("SUBS: todos 0 menos todos 1 (negativo): result = %b,zero,negative,carry,overflow = %b",result,CPSR_FLAGS);
	  
	  a = '1;
	  b = '1;
	  control = 4'b1010;
	  #2
	  $display("ADDS: todos 1: result = %b,zero,negative,carry,overflow = %b",result,CPSR_FLAGS);
	  
	  a = '0;
	  b = '0;
	  control = 4'b1010;
	  $display("ADDS: todos 0: result = %b, zero,negative,carry,overflow = %b",result,CPSR_FLAGS);
	  
	  a = '0;
	  b = '0;
	  control = 4'b1110;
	  $display("SUBS: todos 0: result = %b, zero,negative,carry,overflow = %b",result,CPSR_FLAGS);
	  
	  a = 64'b0;
	  b = 64'b1;
	  control = 4'b1110;
	  #2
	  $display("SUBS: 0 - 1: result = %b,zero,negative,carry,overflow = %b",result,CPSR_FLAGS);
	  
	  a = 64'b1;
	  b = 64'b1;
	  control = 4'b1110;
	  #2
	  $display("SUBS: 1 - 1: result = %b,zero,negative,carry,overflow = %b",result,CPSR_FLAGS);
	  
    end
endmodule  	
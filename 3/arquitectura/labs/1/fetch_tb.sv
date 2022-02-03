/*
- Genere una señal de reloj en el puerto clk del módulo, cuya frecuencia sea 100MHz
(periodo de 10 ns).
- Inicie con la señal reset= ‘1’ durante 5 ciclos de clock y luego coloque reset = ‘0’.
- Inicialice PCbrach_F con un valor fijo.
- Analice que después de colocar reset = ‘0’ el PC inicia en “0” y que después de cada
flanco positivo de clock se actualiza PC= PC+4.
- Coloque PCSrc_F= ‘1’ y en el siguiente flanco positivo de clock el PC tome el valor
inicializado en PCbrach_F .
*/

module fetch_tb ();
	logic clk;
	logic reset = 1;
	logic [63:0] PCBranch_F = '1;
	logic [2:0] counter = 0;
	logic PCSrc_F;
	logic [63:0] imem_addr_F;
	logic [63:0] lastPC = 0;
	
	fetch dut (PCSrc_F,clk,reset,PCBranch_F,imem_addr_F);
	
	always begin 
		clk = 1; #10ns; clk = 0; #10ns;
	end
	always_ff @(posedge clk) begin
		assert(PCBranch_F === lastPC + 4)	
		$display("PC OK");
		if (counter === 3'b101) begin
			reset = !reset;
			counter = 0;
			$display("Reset: %d",reset);
			if (!reset) begin
				assert(PCBranch_F === 0)
				$display("PC OK");
			end
		end
		counter = counter + 1;
		$display("Counter: %d",counter);
		assign lastPC = PCBranch_F;
	end
	
	initial begin 
		#10ns
		$display("Hola");
	end
endmodule 
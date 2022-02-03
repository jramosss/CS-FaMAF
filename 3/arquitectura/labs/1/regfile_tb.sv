/*
- Genere una señal de reloj en el puerto clk del módulo, cuya frecuencia sea 100MHz
(periodo de 10 ns).
- Direccione los registros de 0 a 31 con ra1 y ra2 y verifique los valores de inicialización a la
salida de los dos puertos luego del flanco descendente del clk.
- Escriba un valor en un registro (en el flanco positivo de clock y con we3= ‘1’) y lo lea en el
mismo ciclo de clock, verificando que se actualice correctamente la salida.
- Verifique que no se altere el contenido de un registro si we3= ‘0’.
- Escriba un valor distinto de cero en el registro X31 y verifique que la salida siempre
permanezca en cero.
*/

/*
module regfile_tb ();
	always begin 
		clk = 1; #10ns; clk = 0; #10ns;
	end
	regfile dut ();
	initial begin
		always @ (negedge clk) begin
			
		end
	end
endmodule 
*/
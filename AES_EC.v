module AES_EC(
	
	input clk, n_rst, E_Db,
	input [127:0] pl_text,
	
	output reg [127:0] ci_text,
	output reg done
);


reg [127:0] r_data_in1, r_data_in2, r_key_in = 128'b0;
wire [127:0] w_data_out1, w_data_out2;

AES_enc Enc(.clk(clk), .data_in(r_data_in1), .key(r_key_in), .data_out(w_data_out1));
AES_Decryption Dec(.in(r_data_in2), .key(r_key_in), .out(w_data_out2));

always@(posedge clk or negedge n_rst)
begin

	if(!n_rst)
	begin
	   r_data_in1 <= pl_text;
	   r_data_in2 <= pl_text;
		done <= 1'b0;
	end
	
	else
	begin
	
		if(E_Db)
		begin
			
			r_data_in1 = pl_text;
			ci_text = w_data_out1;
			done = 1'b1;
			
		end
		
		else
		begin
			
			r_data_in2 = pl_text;
			ci_text = w_data_out2;
			done = 1'b1;
			
		end

	end
	
end
endmodule

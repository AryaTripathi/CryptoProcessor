module sha_w_compute(

	input clk,
	input n_rst,
	input init,
	input next,
	
	input [511:0] block_in,
	output [31:0] w_i

);

/* Internal State Definitions ---------------------------------------------------------------------------------------- */

localparam IDLE = 0;
localparam UPDATE = 1;

/* Update Variables and write enable --------------------------------------------------------------------------------- */

reg [31:0] W_mem [15:0];
reg [31:0] W_mem_new [15:0];

reg W_mem_we;

/* Counter Registers and Write Enable -------------------------------------------------------------------------------- */

reg [5:0] W_ctr_reg, W_ctr_new;
reg W_ctr_we;

/* Output Wires ------------------------------------------------------------------------------------------------------ */

reg [31:0] W_temp, W_new;

/* Output Assignment ------------------------------------------------------------------------------------------------- */

assign w_i = W_temp;


/* Update Block ------------------------------------------------------------------------------------------------------ */

always@(posedge clk or negedge n_rst)
begin : update_block
	
	integer i;

	if(!n_rst)
	begin
		
		for(i=0; i<16; i=i+1) W_mem[i] <= 32'h0;
		W_ctr_reg <= 6'h0;

	end

	else
	begin
		if(W_mem_we)
		begin
			for (i = 0; i<16 ; i=i+1) 
			begin
				W_mem[i] <= W_mem_new[i];	
			end
		end

		if (W_ctr_we) 
		begin
			W_ctr_reg <= W_ctr_new;	
		end
	end
end

/* Extracting  W values ---------------------------------------------------------------------------------------------- */

always @(*) 
begin
	if (W_ctr_reg < 16) 
	begin
		W_temp <= W_mem[W_ctr_reg[3:0]];	
	end else begin
		W_temp <= W_new;
	end	
end

/* Calculation of W_new ---------------------------------------------------------------------------------------------- */

always @(*) 
begin : W_calc

	reg [31:0] w_0, w_1, w_9, w_14, s0, s1;

	integer i;
	W_mem_we = 0;

	for (i = 0; i<16; i=i+1) 
	begin
		W_mem_new[i] <= 32'h0;
	end	

	w_0 = W_mem[0];
	w_1 = W_mem[1];
	w_9 = W_mem[9];
	w_14 = W_mem[14];

	s0 = {w_1[6:0], w_1[31:7]} ^ {w_1[17:0], w_1[31:18]} ^ {3'b000, w_1[31:3]};
	s1 = {w_14[16:0], w_14[31:17]} ^ {w_14[18:0], w_14[31:19]} ^ {10'b0000000000, w_1[31:10]};

	W_new = s0 + s1 + w_0 + w_9;

	if (init) 
	begin
		
		W_mem_new[0] = block_in[511:480];
		W_mem_new[1] = block_in[479:448];
		W_mem_new[2] = block_in[447:416];
		W_mem_new[3] = block_in[415:384];

		W_mem_new[4] = block_in[383:352];
		W_mem_new[5] = block_in[351:320];
		W_mem_new[6] = block_in[319:288];
		W_mem_new[7] = block_in[287:256];

		W_mem_new[8] = block_in[255:224];
		W_mem_new[9] = block_in[223:192];
		W_mem_new[10] = block_in[191:160];
		W_mem_new[11] = block_in[159:128];

		W_mem_new[12] = block_in[127:96];
		W_mem_new[13] = block_in[95:64];
		W_mem_new[14] = block_in[63:32];
		W_mem_new[15] = block_in[31:0];

		W_mem_we = 1;

	end 
	
	if (next && (W_ctr_reg > 15)) 
	begin
		for (i = 0; i<15; i=i+1) 
		begin
			W_mem_new[i] = W_mem[i+1];
		end
		W_mem_new[15] = W_new;
		W_mem_we = 1;
	end

end

/* W Control Block Counter ---------------------------------------------------------------------------------------------- */

always @(*) 
begin
	W_ctr_new = 6'h0;
	W_ctr_we = 1'h0;

	if (init)
    begin
        W_ctr_new = 6'h0;
        W_ctr_we  = 1'h1;
    end

	if (next)
    begin
        W_ctr_new = W_ctr_reg + 6'h01;
        W_ctr_we  = 1'h1;
    end
end

endmodule
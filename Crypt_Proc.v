module Crypt_Proc(
	
	input clk, n_rst, E_Db,
	input [3:0] cp_sel_in,
	input [1:0] fn_sel,
	input L_in, L_out,
	
	output reg [7:0] cp_sel_out
);

// --------------------------------------  Register Definitions  --------------------------------------------

reg signed [31:0] cp_in;
reg [31:0] cp_out;
reg [31:0] in_queue [15:0];
reg [31:0] out_queue [7:0];

reg [63:0] r_bf64_out;
reg [255:0] r_sha256_out;
reg [223:0] r_sha224_out;
reg [127:0] r_aes128_out;
reg flag = 0;
reg [5:0] r_count_in = 0;

// AES-128 Registers and Wires :

reg [127:0]r_aes_pl, r_aes_pl_t;
reg r_aes_strt;
wire w_aes_done;
wire [127:0] w_aes_ci;
reg r_aes_in = 0;
reg r_aes_flag = 1;

// Blowfish Registers and Wires :

reg [63:0]r_bf_pl;
reg r_bf_strt;
wire w_bf_done;
wire [63:0] w_bf_ci;
reg r_bf_in = 0;
reg r_bf_flag = 1;

// SHA - 256/224 Registers and Wires :

reg [511:0]r_sha_pl;
reg r_sha_strt;
wire w_sha_done;
wire [255:0] w_sha_hi;
reg r_sha_in = 0;
reg r_sha_flag = 1;
reg r_sha_mode = 1;

integer i;


// --------------------------------------  Module Instantiations  --------------------------------------------


AES_EC Core01(.clk(clk), .n_rst(n_rst & r_aes_strt & r_aes_flag), .E_Db(E_Db), .pl_text(r_aes_pl), .ci_text(w_aes_ci), .done(w_aes_done));

BF_EC Core02(.clk(clk), .n_rst(n_rst & r_bf_strt & r_bf_flag), .E_Db(E_Db), .pl_text(r_bf_pl), .ci_text(w_bf_ci), .bf_done(w_bf_done));

SHA_256_core Core03(.clk(clk), .n_rst(n_rst & r_sha_strt & r_sha_flag), .init(1'b1), .next(1'b1), .mode(r_sha_mode), .block_in(r_sha_pl), .ready(), .valid_digest(w_sha_done), .digest(w_sha_hi));


// --------------------------------------  Queue Input  --------------------------------------------


always@(posedge L_in)
begin
	if(!n_rst)
	begin
		for(i = 0; i < 16; i=i+1)
		begin
			in_queue[i] = 32'h0; 
		end
		r_count_in = 0;
		r_sha_in = 0;
		r_aes_in = 0;
		r_bf_in = 0;
		
	end
	
	else
	begin
		for(i=14;i>=0;i=i-1)
		begin
			in_queue[i+1] = in_queue[i];
		end
		in_queue[0] = cp_in;
		r_count_in = r_count_in + 1;
		
		if(r_count_in == 5'b00010) r_bf_in = 1;
		if(r_count_in == 5'b00100) r_aes_in = 1;
		if(r_count_in == 5'b10000)
		begin
			r_sha_in = 1;
			r_count_in = 0;
		end
		
	end
end


// --------------------------------------  Queue Output  --------------------------------------------


always@(posedge L_out)
begin
	if(!n_rst)
	begin
		for(i = 0; i < 8; i=i+1)
		begin
			out_queue[i] = 32'h0; 
			flag = 1'b0;
		end
	end
	
	else
	begin
		
		case({flag,fn_sel})
		
		3'b000 : 
		begin
			out_queue[7] <= r_aes128_out[127:96];
			out_queue[6] <= r_aes128_out[95:64];
			out_queue[5] <= r_aes128_out[63:32];
			out_queue[4] <= r_aes128_out[31:0];
			flag <= 1'b1;
		end
		
		3'b001 : 
		begin
			out_queue[7] <= r_bf64_out[63:32];
			out_queue[6] <= r_bf64_out[31:0];
			flag <= 1'b1;
		end
		
		3'b010 : 
		begin
			out_queue[7] <= r_sha256_out[255:224];
			out_queue[6] <= r_sha256_out[223:192];
			out_queue[5] <= r_sha256_out[191:160];
			out_queue[4] <= r_sha256_out[159:128];
			
			out_queue[3] <= r_sha256_out[127:96];
			out_queue[2] <= r_sha256_out[95:64];
			out_queue[1] <= r_sha256_out[63:32];	
			out_queue[0] <= r_sha256_out[31:0];
			flag <= 1'b1;
		end
		
		3'b011 : 
		begin
			out_queue[7] <= r_sha224_out[223:192];
			out_queue[6] <= r_sha224_out[191:160];
			out_queue[5] <= r_sha224_out[159:128];
			out_queue[4] <= r_sha224_out[127:96];
		
			out_queue[3] <= r_sha224_out[95:64];
			out_queue[2] <= r_sha224_out[63:32];
			out_queue[1] <= r_sha224_out[31:0];
			flag <= 1'b1;
		end
		
		default :
		begin
			cp_out = out_queue[7];
			cp_sel_out = cp_out[7:0];
			for(i=6;i>=0;i=i-1)
			begin
				out_queue[i+1] = out_queue[i];
			end
		
		end
		endcase
		
	end
end


// --------------------------------------  Queue Update  --------------------------------------------


always@(posedge clk or negedge n_rst)
begin
		
	if(!n_rst)
	begin
		
		r_aes_pl <= 0;
		r_aes_strt <= 0;
		r_aes_flag <= 1;
		
		r_bf_pl <= 0;
		r_bf_strt <= 0;
		r_bf_flag <= 1;
		
		r_sha_pl <= 0;
		r_sha_strt <= 0;
		r_sha_flag <= 1;
		
	
	end
	
	else
	
	case(fn_sel)
	
	2'b00 :													// AES 128 Encryption and Decryption
	begin
	
		r_aes_pl_t[127:96] = in_queue[3];
		r_aes_pl_t[95:64] = in_queue[2];
		r_aes_pl_t[63:32] = in_queue[1];
		r_aes_pl_t[31:0] = in_queue[0];
		
		if(r_aes_in)
		begin	
			r_aes_strt = 1;
			r_aes_pl = r_aes_pl_t;
		end
		
		
		if(w_aes_done)
		begin
			r_aes128_out = w_aes_ci;
			r_aes_strt = 0;
			r_aes_flag = 0;
		end
	
	end
	
	2'b01 :													// Blowfish 64 Encryption and Decryption
	begin
				
		if(r_bf_in)
		begin
		  r_bf_pl[63:32] = in_queue[2];
		  r_bf_pl[31:0] = in_queue[1];
		  r_bf_strt = 1;
		 end
		
		if(w_bf_done)
		begin
			r_bf64_out = w_bf_ci;
			r_bf_strt = 0;
			r_bf_flag = 0;
		end
	end
	
	2'b10 :													// SHA - 256 Hashing
	begin
		r_sha_pl[511:480] = in_queue[15];
		r_sha_pl[479:448] = in_queue[14];
		r_sha_pl[447:416] = in_queue[13];
		r_sha_pl[415:384] = in_queue[12];
		
		r_sha_pl[383:352] = in_queue[11];
		r_sha_pl[351:320] = in_queue[10];
		r_sha_pl[319:288] = in_queue[9];
		r_sha_pl[287:256] = in_queue[8];
		
		r_sha_pl[255:224] = in_queue[7];
		r_sha_pl[223:192] = in_queue[6];
		r_sha_pl[191:160] = in_queue[5];
		r_sha_pl[159:128] = in_queue[4];
		
		r_sha_pl[127:96] = in_queue[3];
		r_sha_pl[95:64] = in_queue[2];		
		r_sha_pl[63:32] = in_queue[1];
		r_sha_pl[31:0] = in_queue[0];
		
		r_sha_mode = 1;
		
		if(r_sha_in) r_sha_strt = 1;
		
		if(w_sha_done)
		begin
			r_sha256_out = w_sha_hi;
			r_sha_strt = 0;
			r_sha_flag = 0;
		end
	
	end
	
	2'b11 :													// SHA - 224 Hashing
	begin
		
		r_sha_pl[511:480] = in_queue[15];
		r_sha_pl[479:448] = in_queue[14];
		r_sha_pl[447:416] = in_queue[13];
		r_sha_pl[415:384] = in_queue[12];
		
		r_sha_pl[383:352] = in_queue[11];
		r_sha_pl[351:320] = in_queue[10];
		r_sha_pl[319:288] = in_queue[9];
		r_sha_pl[287:256] = in_queue[8];
		
		r_sha_pl[255:224] = in_queue[7];
		r_sha_pl[223:192] = in_queue[6];
		r_sha_pl[191:160] = in_queue[5];
		r_sha_pl[159:128] = in_queue[4];
		
		r_sha_pl[127:96] = in_queue[3];
		r_sha_pl[95:64] = in_queue[2];		
		r_sha_pl[63:32] = in_queue[1];
		r_sha_pl[31:0] = in_queue[0];
		
		r_sha_mode = 0;
		
		if(r_sha_in) r_sha_strt = 1;
		
		if(w_sha_done)
		begin
			r_sha224_out = w_sha_hi[223:0];
			r_sha_strt = 0;
			r_sha_flag = 0;
		end
	
	end
	endcase
end

always@(posedge clk)
begin

    cp_in[31:28] = cp_sel_in;
    cp_in = cp_in >>> 28;
    
end

endmodule

module SHA_256_core(

    input clk,
    input n_rst,
    
    input init,
    input next,
    input mode,

    input [511:0] block_in,

    output ready, 
    output valid_digest,
    output [255:0] digest
);

  parameter SHA256_H0_0 = 32'h6a09e667;
  parameter SHA256_H0_1 = 32'hbb67ae85;
  parameter SHA256_H0_2 = 32'h3c6ef372;
  parameter SHA256_H0_3 = 32'ha54ff53a;
  parameter SHA256_H0_4 = 32'h510e527f;
  parameter SHA256_H0_5 = 32'h9b05688c;
  parameter SHA256_H0_6 = 32'h1f83d9ab;
  parameter SHA256_H0_7 = 32'h5be0cd19;

  parameter SHA224_H0_0 = 32'hc1059ed8;
  parameter SHA224_H0_1 = 32'h367cd507;
  parameter SHA224_H0_2 = 32'h3070dd17;
  parameter SHA224_H0_3 = 32'hf70e5939;
  parameter SHA224_H0_4 = 32'hffc00b31;
  parameter SHA224_H0_5 = 32'h68581511;
  parameter SHA224_H0_6 = 32'h64f98fa7;
  parameter SHA224_H0_7 = 32'hbefa4fa4;

  parameter SHA_rounds = 64;

  parameter IDLE = 0;
  parameter ROUND_RUN = 1;
  parameter DONE = 2;

/* Update Variables and Registers ---------------------------------------------------------------------*/

  reg [31:0] reg_mem [7:0];
  reg [31:0] reg_mem_new [7:0];

  reg [31:0] H_reg [7:0];
  reg [31:0] H_new [7:0];

  reg a_h_we, H_we;

  reg [5:0] t_ctr_reg, t_ctr_new;
  reg t_ctr_we, t_ctr_inc, t_ctr_rst;

  reg valid_digest_reg, valid_digest_new, valid_digest_we;

  reg [1:0] sha256_ctrl_reg, sha256_ctrl_new;
  reg sha256_ctrl_we;

  reg digest_init, digest_update;
  reg state_init, state_update;

  reg first_b, ready_flag;
  reg [31:0] T1, T2;
  reg w_init, w_next;

  wire [31:0] k_data, w_data;
  
/* K Constants and W Mem Access */

  sha_k_const k_cont(.addr(t_ctr_reg), .K_value(k_data));
  sha_w_compute w_mem(.clk(clk), .n_rst(n_rst), .block_in(block_in), .init(w_init), .next(w_next), .w_i(w_data));

/* Port Connections */

assign ready = ready_flag;
assign digest = {H_reg[0], H_reg[1], H_reg[2], H_reg[3], H_reg[4], H_reg[5], H_reg[6], H_reg[7]};
assign valid_digest = valid_digest_reg;

/* Core Register Updation */

always @(posedge clk or negedge n_rst) 
begin : Reg_Update
    if (!n_rst) 
    begin : rst_cond
		  integer i;
        for(i = 0; i<8; i=i+1)
        begin : rst
            reg_mem[i] <= 32'h0;
            H_reg[i] <= 32'h0;
        end    

        valid_digest_reg <= 0;
        t_ctr_reg <= 6'h0;
        sha256_ctrl_reg <= IDLE;

    end 
    
    else 
    begin
        if (a_h_we) 
        begin : write_en
            integer j;
            for (j = 0; j<8 ; j=j+1) 
            begin
                reg_mem[j] <= reg_mem_new[j];    
            end    
        end

        if (H_we) 
        begin : write_en2
            integer k;
            for (k = 0; k<8 ; k=k+1) 
            begin
                H_reg[k] <= H_new[k];    
            end    
        end
	 
	 if(t_ctr_we) t_ctr_reg <= t_ctr_new;

    if(valid_digest_we) valid_digest_reg <= valid_digest_new;

    if(sha256_ctrl_we) sha256_ctrl_reg <= sha256_ctrl_new; 
	 
    end  

end

/* Digest Computation Logic */

always @(*) 
begin : Digest_Compute
	 integer i;
    for (i = 0; i<8 ; i=i+1) 
    begin : rst
        H_new[i] = 0;    
    end
    H_we = 0;

    if (digest_init) 
    begin
        H_we = 1;
        if (mode) 
        begin
            H_new[0] = SHA256_H0_0;
            H_new[1] = SHA256_H0_1;
            H_new[2] = SHA256_H0_2;
            H_new[3] = SHA256_H0_3;
            H_new[4] = SHA256_H0_4;
            H_new[5] = SHA256_H0_5;
            H_new[6] = SHA256_H0_6;
            H_new[7] = SHA256_H0_7;
        end

        else
        begin
            H_new[0] = SHA224_H0_0;
            H_new[1] = SHA224_H0_1;
            H_new[2] = SHA224_H0_2;
            H_new[3] = SHA224_H0_3;
            H_new[4] = SHA224_H0_4;
            H_new[5] = SHA224_H0_5;
            H_new[6] = SHA224_H0_6;
            H_new[7] = SHA224_H0_7;
        end
    end

    if(digest_update)
    begin
        for (i = 0; i<8 ; i=i+1) 
        begin
            H_new[i] = H_reg[i] + reg_mem[i];    
        end
        H_we = 1;
    end
end


/* T1 and T2 Update Blocks */

always @(*) 
begin : T1_Compute

    reg [31:0] sum_1, ch;

    sum_1 = {reg_mem[4][5:0], reg_mem[4][31:6]} ^ {reg_mem[4][10:0], reg_mem[4][31:11]} ^ {reg_mem[4][24:0], reg_mem[4][31:25]};

    ch = (reg_mem[4] & reg_mem[5]) ^ ((~reg_mem[4]) & reg_mem[6]);

    T1 = reg_mem[7] + sum_1 + ch + w_data + k_data;

end

always @(*) 
begin : T2_Compute

    reg [31:0] sum_0, maj;

    sum_0 = {reg_mem[0][1:0], reg_mem[0][31:2]} ^ {reg_mem[0][12:0], reg_mem[0][31:13]} ^ {reg_mem[0][21:0], reg_mem[0][31:22]};

    maj = (reg_mem[0] & reg_mem[1]) ^ (reg_mem[0] & reg_mem[2]) ^ (reg_mem[1] & reg_mem[2]);

    T2 = sum_0 + maj;

end

/* State Transition Logic */

always @(*) 
begin : State_Transit
    integer i;
    for (i = 0; i<8 ; i=i+1) 
    begin
        reg_mem_new[i] = 32'h0;    
    end
    a_h_we = 0;

    if(state_init)
    begin
        a_h_we = 1;
        if(first_b)
        begin
            if(mode)
            begin
                reg_mem_new[0] = SHA256_H0_0;
                reg_mem_new[1] = SHA256_H0_1;
                reg_mem_new[2] = SHA256_H0_2;
                reg_mem_new[3] = SHA256_H0_3;
                reg_mem_new[4] = SHA256_H0_4;
                reg_mem_new[5] = SHA256_H0_5;
                reg_mem_new[6] = SHA256_H0_6;
                reg_mem_new[7] = SHA256_H0_7;    
            end
            
            else
            begin
                reg_mem_new[0] = SHA224_H0_0;
                reg_mem_new[1] = SHA224_H0_1;
                reg_mem_new[2] = SHA224_H0_2;
                reg_mem_new[3] = SHA224_H0_3;
                reg_mem_new[4] = SHA224_H0_4;
                reg_mem_new[5] = SHA224_H0_5;
                reg_mem_new[6] = SHA224_H0_6;
                reg_mem_new[7] = SHA224_H0_7;
            end
        end

        else 
        begin : next_blocks
            integer j;
            for (j = 0; j<8 ; j=j+1) 
            begin
                reg_mem_new[j] = H_reg[j];    
            end
        end

    end

    if (state_update) 
    begin
        reg_mem_new[0] = T1 + T2;
        reg_mem_new[1] = reg_mem[0];
        reg_mem_new[2] = reg_mem[1];
        reg_mem_new[3] = reg_mem[2];
        reg_mem_new[4] = reg_mem[3] + T1;
        reg_mem_new[5] = reg_mem[4];
        reg_mem_new[6] = reg_mem[5];
        reg_mem_new[7] = reg_mem[6];    
        a_h_we = 1;
    end    
end

/* Counter Update Logic */

always @(*) 
begin : Counter_Update
    t_ctr_new = 0;
    t_ctr_we = 0;

    if (t_ctr_rst) 
    begin
        t_ctr_new = 0;
        t_ctr_we = 1;    
    end    

    if (t_ctr_inc) 
    begin
        t_ctr_new = t_ctr_reg + 1'b1;
        t_ctr_we = 1;    
    end
end

/* State Machine */

always @(*) 
begin : State_Machine
    digest_init = 0;
    digest_update = 0;
    state_init = 0;
    state_update = 0;

    first_b = 0;
    ready_flag = 0;
    w_init = 0;
    w_next = 0;

    t_ctr_inc = 0;
    t_ctr_rst = 0;
    valid_digest_new = 0;
    valid_digest_we = 0;

    sha256_ctrl_we = 0;
    sha256_ctrl_new = IDLE;

    case (sha256_ctrl_reg)
       
        IDLE : 
        begin
            ready_flag = 1;

            if (init) 
            begin
                digest_init = 1;
                w_init = 1;
                state_init = 1;
                first_b = 1;
                t_ctr_rst = 1;
                valid_digest_new = 0;
                valid_digest_we = 1;
                sha256_ctrl_we = 1;

                sha256_ctrl_new = ROUND_RUN;    
            end

            if (next) 
            begin
                t_ctr_rst = 1;
                w_init = 1;
                state_init = 1;
                valid_digest_new = 0;
                valid_digest_we = 1;
                sha256_ctrl_we = 1;

                sha256_ctrl_new = ROUND_RUN;    
            end
        end

        ROUND_RUN :
        begin
            w_next = 1;
            state_update = 1;
            t_ctr_inc = 1;

            if (t_ctr_reg == SHA_rounds - 1) 
            begin
                sha256_ctrl_we = 1;    

                sha256_ctrl_new = DONE;
            end
        end
        
        DONE :
        begin
            digest_update = 1;
            valid_digest_new = 1;
            valid_digest_we = 1;
            sha256_ctrl_we = 1;

            sha256_ctrl_new = IDLE;
        end
    endcase

end

endmodule
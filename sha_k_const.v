module sha_k_const(

	input [5:0] addr,
	output [31:0] K_value

);

reg [31:0] temp_K_reg;
assign K_value = temp_K_reg;

always@(addr)
begin
	
	case(addr)
	
		00 : temp_K_reg = 32'h428a2f98;
      01 : temp_K_reg = 32'h71374491;
      02 : temp_K_reg = 32'hb5c0fbcf;
      03 : temp_K_reg = 32'he9b5dba5;
      04 : temp_K_reg = 32'h3956c25b;
      05 : temp_K_reg = 32'h59f111f1;
      06 : temp_K_reg = 32'h923f82a4;
      07 : temp_K_reg = 32'hab1c5ed5;
      08 : temp_K_reg = 32'hd807aa98;
      09 : temp_K_reg = 32'h12835b01;
      10 : temp_K_reg = 32'h243185be;
      11 : temp_K_reg = 32'h550c7dc3;
      12 : temp_K_reg = 32'h72be5d74;
      13 : temp_K_reg = 32'h80deb1fe;
      14 : temp_K_reg = 32'h9bdc06a7;
      15 : temp_K_reg = 32'hc19bf174;
      16 : temp_K_reg = 32'he49b69c1;
      17 : temp_K_reg = 32'hefbe4786;
      18 : temp_K_reg = 32'h0fc19dc6;
      19 : temp_K_reg = 32'h240ca1cc;
      20 : temp_K_reg = 32'h2de92c6f;
      21 : temp_K_reg = 32'h4a7484aa;
      22 : temp_K_reg = 32'h5cb0a9dc;
      23 : temp_K_reg = 32'h76f988da;
      24 : temp_K_reg = 32'h983e5152;
      25 : temp_K_reg = 32'ha831c66d;
      26 : temp_K_reg = 32'hb00327c8;
      27 : temp_K_reg = 32'hbf597fc7;
      28 : temp_K_reg = 32'hc6e00bf3;
      29 : temp_K_reg = 32'hd5a79147;
      30 : temp_K_reg = 32'h06ca6351;
      31 : temp_K_reg = 32'h14292967;
      32 : temp_K_reg = 32'h27b70a85;
      33 : temp_K_reg = 32'h2e1b2138;
      34 : temp_K_reg = 32'h4d2c6dfc;
      35 : temp_K_reg = 32'h53380d13;
      36 : temp_K_reg = 32'h650a7354;
      37 : temp_K_reg = 32'h766a0abb;
      38 : temp_K_reg = 32'h81c2c92e;
      39 : temp_K_reg = 32'h92722c85;
      40 : temp_K_reg = 32'ha2bfe8a1;
      41 : temp_K_reg = 32'ha81a664b;
      42 : temp_K_reg = 32'hc24b8b70;
      43 : temp_K_reg = 32'hc76c51a3;
      44 : temp_K_reg = 32'hd192e819;
      45 : temp_K_reg = 32'hd6990624;
      46 : temp_K_reg = 32'hf40e3585;
      47 : temp_K_reg = 32'h106aa070;
      48 : temp_K_reg = 32'h19a4c116;
      49 : temp_K_reg = 32'h1e376c08;
      50 : temp_K_reg = 32'h2748774c;
      51 : temp_K_reg = 32'h34b0bcb5;
      52 : temp_K_reg = 32'h391c0cb3;
      53 : temp_K_reg = 32'h4ed8aa4a;
      54 : temp_K_reg = 32'h5b9cca4f;
      55 : temp_K_reg = 32'h682e6ff3;
      56 : temp_K_reg = 32'h748f82ee;
      57 : temp_K_reg = 32'h78a5636f;
      58 : temp_K_reg = 32'h84c87814;
      59 : temp_K_reg = 32'h8cc70208;
      60 : temp_K_reg = 32'h90befffa;
      61 : temp_K_reg = 32'ha4506ceb;
      62 : temp_K_reg = 32'hbef9a3f7;
      63 : temp_K_reg = 32'hc67178f2;
		
	endcase

end

endmodule

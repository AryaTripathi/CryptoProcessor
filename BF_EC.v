module BF_EC(

input clk, n_rst, E_Db,
input [63:0] pl_text,

output reg [63:0] ci_text,
output reg bf_done

);

// ------------------------------------  Parameter Declaration ---------------------------------------

parameter IDLE = 3'b000;
parameter ROUND_NO = 3'b001;
parameter ROUND_NE = 3'b010;
parameter BF_POST_PRO = 3'b011;
parameter DONE = 3'b100;

// ------------------------------------  Register Declaration ---------------------------------------

reg [31:0] P_mem [17:0];
reg [2:0] state_curr, state_next;
reg [31:0] r_XL, r_XR;
reg [31:0] r_XL_t, r_XR_t;
reg [31:0] r_sb1, r_sb2, r_sb3, r_sb4;
reg [3:0] r_round;
reg [31:0] r_f_out;
reg [31:0] r_P_af;

reg [31:0] s_mem [2**10 - 1:0];


// ------------------------------------  State Update ---------------------------------------

always@(posedge clk or negedge n_rst)
begin
	
	if(!n_rst)
	begin
	
		
		P_mem[5'h00] <= 32'h243f6a88;
		P_mem[5'h01] <= 32'h85a308d3;
		P_mem[5'h02] <= 32'h13198a2e;
		P_mem[5'h03] <= 32'h03707344;
		
		P_mem[5'h04] <= 32'ha4093822;
		P_mem[5'h05] <= 32'h299f31d0;
		P_mem[5'h06] <= 32'h082efa98;
		P_mem[5'h07] <= 32'hec4e6c89;
		
		P_mem[5'h08] <= 32'h452821e6;
		P_mem[5'h09] <= 32'h38d01377;
		P_mem[5'h0a] <= 32'hbe5466cf;
		P_mem[5'h0b] <= 32'h34e90c6c;
		
		P_mem[5'h0c] <= 32'hc0ac29b7;
		P_mem[5'h0d] <= 32'hc97c50dd;
		P_mem[5'h0e] <= 32'h3f84d5b5;
		P_mem[5'h0f] <= 32'hb5470917;
		
		P_mem[5'h10] <= 32'h9216d5d9;
		P_mem[5'h11] <= 32'h8979fb1b;
		
		state_curr <= IDLE;
	end
	
	else
	begin
		
		state_curr <= state_next;
	
	end

end

// ------------------------------------  State Update Logic ---------------------------------------

always@(*)
begin
	
	case(state_curr)
	
	IDLE :
	begin
		
		r_XL = pl_text[63:32];
		r_XR = pl_text[31:0];
		bf_done = 0;
		r_round = 4'b0000;
		state_next = ROUND_NO;
	
	end
	
	ROUND_NO :
	begin
		
		if(r_round == 4'b0000)
		begin
			r_XL = pl_text[63:32];
			r_XR = pl_text[31:0];
		end
		
		else
		begin
			r_XL = r_XL_t;
			r_XR = r_XR_t;
		end
		
		if(E_Db) r_P_af = r_XL ^ P_mem[{1'b0,r_round}];
		else r_P_af = r_XL ^ P_mem[17 - r_round];
		
		r_XR_t = r_P_af;
		
		r_sb1 = s_mem[{2'b00, r_P_af[31:24]}];
		r_sb2 = s_mem[{2'b01, r_P_af[23:16]}];
		r_sb3 = s_mem[{2'b10, r_P_af[15:8]}];
		r_sb4 = s_mem[{2'b11, r_P_af[7:0]}];
		
		r_f_out = Feistal(r_sb1, r_sb2, r_sb3, r_sb4);
		
		r_XL_t = r_f_out ^ r_XR;
		
		r_round = r_round + 4'b0001;
		
		state_next = ROUND_NE;
			
	end
	
	ROUND_NE :
	begin
		
		if(r_round == 4'b0000)
		begin
			r_XL = pl_text[63:32];
			r_XR = pl_text[31:0];
		end
		
		else
		begin
			r_XL = r_XL_t;
			r_XR = r_XR_t;
		end
		
		if(E_Db) r_P_af = r_XL ^ P_mem[{1'b0,r_round}];
		else r_P_af = r_XL ^ P_mem[17 - r_round];
		
		r_XR_t = r_P_af;
				
		r_sb1 = s_mem[{2'b00, r_P_af[31:24]}];
		r_sb2 = s_mem[{2'b01, r_P_af[23:16]}];
		r_sb3 = s_mem[{2'b10, r_P_af[15:8]}];
		r_sb4 = s_mem[{2'b11, r_P_af[7:0]}];
		
		r_f_out = Feistal(r_sb1, r_sb2, r_sb3, r_sb4);
		
		r_XL_t = r_f_out ^ r_XR;
		
		r_round = r_round + 4'b0001;
		
		if(r_round == 4'b0000)
			state_next = BF_POST_PRO;
		else
			state_next = ROUND_NO;
			
	end
	
	BF_POST_PRO :
	begin
		r_XR = r_XR_t;
		r_XL = r_XL_t;
		
		if(E_Db)
		begin
			r_XR_t = r_XL ^ P_mem[16];
			r_XL_t = r_XR ^ P_mem[17];
		end
		
		else
		begin
			r_XR_t = r_XL ^ P_mem[1];
			r_XL_t = r_XR ^ P_mem[0];
		end
		
		r_XR = r_XR_t;
		r_XL = r_XL_t;
		
		state_next = DONE;
	
	end
	
	DONE :
	begin
		ci_text = {r_XL, r_XR};
		bf_done = 1;
	end
	
	endcase
	
end

// ------------------------------------  S_BOX Memory ---------------------------------------

always@(posedge clk or negedge n_rst)
begin

	if(!n_rst)
	begin
		
		// S BOX 01
		
		s_mem[10'h000] <= 32'hd1310ba6;
		s_mem[10'h001] <= 32'h98dfb5ac;
		s_mem[10'h002] <= 32'h2ffd72db;
		s_mem[10'h003] <= 32'hd01adfb7;
		s_mem[10'h004] <= 32'hb8e1afed;
		s_mem[10'h005] <= 32'h6a267e96;
		s_mem[10'h006] <= 32'hba7c9045;
		s_mem[10'h007] <= 32'hf12c7f99;
		s_mem[10'h008] <= 32'h24a19947;
		s_mem[10'h009] <= 32'hb3916cf7;
		s_mem[10'h00A] <= 32'h0801f2e2;
		s_mem[10'h00B] <= 32'h858efc16;
		s_mem[10'h00C] <= 32'h636920d8;
		s_mem[10'h00D] <= 32'h71574e69;
		s_mem[10'h00E] <= 32'ha458fea3;
		s_mem[10'h00F] <= 32'hf4933d7e;
		s_mem[10'h010] <= 32'h0d95748f;
		s_mem[10'h011] <= 32'h728eb658;
		s_mem[10'h012] <= 32'h718bcd58;
		s_mem[10'h013] <= 32'h82154aee;
		s_mem[10'h014] <= 32'h7b54a41d;
		s_mem[10'h015] <= 32'hc25a59b5;
		s_mem[10'h016] <= 32'h9c30d539;
		s_mem[10'h017] <= 32'h2af26013;
		s_mem[10'h018] <= 32'hc5d1b023;
		s_mem[10'h019] <= 32'h286085f0;
		s_mem[10'h01A] <= 32'hca417918;
		s_mem[10'h01B] <= 32'hb8db38ef;
		s_mem[10'h01C] <= 32'h8e79dcb0;
		s_mem[10'h01D] <= 32'h603a180e;
		s_mem[10'h01E] <= 32'h6c9e0e8b;
		s_mem[10'h01F] <= 32'hb01e8a3e;
		s_mem[10'h020] <= 32'hd71577c1;
		s_mem[10'h021] <= 32'hbd314b27;
		s_mem[10'h022] <= 32'h78af2fda;
		s_mem[10'h023] <= 32'h55605c60;
		s_mem[10'h024] <= 32'he65525f3;
		s_mem[10'h025] <= 32'haa55ab94;
		s_mem[10'h026] <= 32'h57489862;
		s_mem[10'h027] <= 32'h63e81440;
		s_mem[10'h028] <= 32'h55ca396a;
		s_mem[10'h029] <= 32'h2aab10b6;
		s_mem[10'h02A] <= 32'hb4cc5c34;
		s_mem[10'h02B] <= 32'h1141e8ce;
		s_mem[10'h02C] <= 32'ha15486af;
		s_mem[10'h02D] <= 32'h7c72e993;
		s_mem[10'h02E] <= 32'hb3ee1411;
		s_mem[10'h02F] <= 32'h636fbc2a;
		s_mem[10'h030] <= 32'h2ba9c55d;
		s_mem[10'h031] <= 32'h741831f6;
		s_mem[10'h032] <= 32'hce5c3e16;
		s_mem[10'h033] <= 32'h9b87931e;
		s_mem[10'h034] <= 32'hafd6ba33;
		s_mem[10'h035] <= 32'h6c24cf5c;
		s_mem[10'h036] <= 32'h7a325381;
		s_mem[10'h037] <= 32'h28958677;
		s_mem[10'h038] <= 32'h3b8f4898;
		s_mem[10'h039] <= 32'h6b4bb9af;
		s_mem[10'h03A] <= 32'hc4bfe81b;
		s_mem[10'h03B] <= 32'h66282193;
		s_mem[10'h03C] <= 32'h61d809cc;
		s_mem[10'h03D] <= 32'hfb21a991;
		s_mem[10'h03E] <= 32'h487cac60;
		s_mem[10'h03F] <= 32'h5dec8032;
		s_mem[10'h040] <= 32'hef845d5d;
		s_mem[10'h041] <= 32'he98575b1;
		s_mem[10'h042] <= 32'hdc262302;
		s_mem[10'h043] <= 32'heb651b88;
		s_mem[10'h044] <= 32'h23893e81;
		s_mem[10'h045] <= 32'hd396acc5;
		s_mem[10'h046] <= 32'h0f6d6ff3;
		s_mem[10'h047] <= 32'h83f44239;
		s_mem[10'h048] <= 32'h2e0b4482;
		s_mem[10'h049] <= 32'ha4842004;
		s_mem[10'h04A] <= 32'h69c8f04a;
		s_mem[10'h04B] <= 32'h9e1f9b5e;
		s_mem[10'h04C] <= 32'h21c66842;
		s_mem[10'h04D] <= 32'hf6e96c9a;
		s_mem[10'h04E] <= 32'h670c9c61;
		s_mem[10'h04F] <= 32'habd388f0;
		s_mem[10'h050] <= 32'h6a51a0d2;
		s_mem[10'h051] <= 32'hd8542f68;
		s_mem[10'h052] <= 32'h960fa728;
		s_mem[10'h053] <= 32'hab5133a3;
		s_mem[10'h054] <= 32'h6eef0b6c;
		s_mem[10'h055] <= 32'h137a3be4;
		s_mem[10'h056] <= 32'hba3bf050;
		s_mem[10'h057] <= 32'h7efb2a98;
		s_mem[10'h058] <= 32'ha1f1651d;
		s_mem[10'h059] <= 32'h39af0176;
		s_mem[10'h05A] <= 32'h66ca593e;
		s_mem[10'h05B] <= 32'h82430e88;
		s_mem[10'h05C] <= 32'h8cee8619;
		s_mem[10'h05D] <= 32'h456f9fb4;
		s_mem[10'h05E] <= 32'h7d84a5c3;
		s_mem[10'h05F] <= 32'h3b8b5ebe;
		s_mem[10'h060] <= 32'he06f75d8;
		s_mem[10'h061] <= 32'h85c12073;
		s_mem[10'h062] <= 32'h401a449f;
		s_mem[10'h063] <= 32'h56c16aa6;
		s_mem[10'h064] <= 32'h4ed3aa62;
		s_mem[10'h065] <= 32'h363f7706;
		s_mem[10'h066] <= 32'h1bfedf72;
		s_mem[10'h067] <= 32'h429b023d;
		s_mem[10'h068] <= 32'h37d0d724;
		s_mem[10'h069] <= 32'hd00a1248;
		s_mem[10'h06A] <= 32'hdb0fead3;
		s_mem[10'h06B] <= 32'h49f1c09b;
		s_mem[10'h06C] <= 32'h075372c9;
		s_mem[10'h06D] <= 32'h80991b7b;
		s_mem[10'h06E] <= 32'h25d479d8;
		s_mem[10'h06F] <= 32'hf6e8def7;
		s_mem[10'h070] <= 32'he3fe501a;
		s_mem[10'h071] <= 32'hb6794c3b;
		s_mem[10'h072] <= 32'h976ce0bd;
		s_mem[10'h073] <= 32'h04c006ba;
		s_mem[10'h074] <= 32'hc1a94fb6;
		s_mem[10'h075] <= 32'h409f60c4;
		s_mem[10'h076] <= 32'h5e5c9ec2;
		s_mem[10'h077] <= 32'h196a2463;
		s_mem[10'h078] <= 32'h68fb6faf;
		s_mem[10'h079] <= 32'h3e6c53b5;
		s_mem[10'h07A] <= 32'h1339b2eb;
		s_mem[10'h07B] <= 32'h3b52ec6f;
		s_mem[10'h07C] <= 32'h6dfc511f;
		s_mem[10'h07D] <= 32'h9b30952c;
		s_mem[10'h07E] <= 32'hcc814544;
		s_mem[10'h07F] <= 32'haf5ebd09;
		s_mem[10'h080] <= 32'hbee3d004;
		s_mem[10'h081] <= 32'hde334afd;
		s_mem[10'h082] <= 32'h660f2807;
		s_mem[10'h083] <= 32'h192e4bb3;
		s_mem[10'h084] <= 32'hc0cba857;
		s_mem[10'h085] <= 32'h45c8740f;
		s_mem[10'h086] <= 32'hd20b5f39;
		s_mem[10'h087] <= 32'hb9d3fbdb;
		s_mem[10'h088] <= 32'h5579c0bd;
		s_mem[10'h089] <= 32'h1a60320a;
		s_mem[10'h08A] <= 32'hd6a100c6;
		s_mem[10'h08B] <= 32'h402c7279;
		s_mem[10'h08C] <= 32'h679f25fe;
		s_mem[10'h08D] <= 32'hfb1fa3cc;
		s_mem[10'h08E] <= 32'h8ea5e9f8;
		s_mem[10'h08F] <= 32'hdb3222f8;
		s_mem[10'h090] <= 32'h3c7516df;
		s_mem[10'h091] <= 32'hfd616b15;
		s_mem[10'h092] <= 32'h2f501ec8;
		s_mem[10'h093] <= 32'had0552ab;
		s_mem[10'h094] <= 32'h323db5fa;
		s_mem[10'h095] <= 32'hfd238760;
		s_mem[10'h096] <= 32'h53317b48;
		s_mem[10'h097] <= 32'h3e00df82;
		s_mem[10'h098] <= 32'h9e5c57bb;
		s_mem[10'h099] <= 32'hca6f8ca0;
		s_mem[10'h09A] <= 32'h1a87562e;
		s_mem[10'h09B] <= 32'hdf1769db;
		s_mem[10'h09C] <= 32'hd542a8f6;
		s_mem[10'h09D] <= 32'h287effc3;
		s_mem[10'h09E] <= 32'hac6732c6;
		s_mem[10'h09F] <= 32'h8c4f5573;
		s_mem[10'h0A0] <= 32'h695b27b0;
		s_mem[10'h0A1] <= 32'hbbca58c8;
		s_mem[10'h0A2] <= 32'he1ffa35d;
		s_mem[10'h0A3] <= 32'hb8f011a0;
		s_mem[10'h0A4] <= 32'h10fa3d98;
		s_mem[10'h0A5] <= 32'hfd2183b8;
		s_mem[10'h0A6] <= 32'h4afcb56c;
		s_mem[10'h0A7] <= 32'h2dd1d35b;
		s_mem[10'h0A8] <= 32'h9a53e479;
		s_mem[10'h0A9] <= 32'hb6f84565;
		s_mem[10'h0AA] <= 32'hd28e49bc;
		s_mem[10'h0AB] <= 32'h4bfb9790;
		s_mem[10'h0AC] <= 32'he1ddf2da;
		s_mem[10'h0AD] <= 32'ha4cb7e33;
		s_mem[10'h0AE] <= 32'h62fb1341;
		s_mem[10'h0AF] <= 32'hcee4c6e8;
		s_mem[10'h0B0] <= 32'hef20cada;
		s_mem[10'h0B1] <= 32'h36774c01;
		s_mem[10'h0B2] <= 32'hd07e9efe;
		s_mem[10'h0B3] <= 32'h2bf11fb4;
		s_mem[10'h0B4] <= 32'h95dbda4d;
		s_mem[10'h0B5] <= 32'hae909198;
		s_mem[10'h0B6] <= 32'heaad8e71;
		s_mem[10'h0B7] <= 32'h6b93d5a0;
		s_mem[10'h0B8] <= 32'hd08ed1d0;
		s_mem[10'h0B9] <= 32'hafc725e0;
		s_mem[10'h0BA] <= 32'h8e3c5b2f;
		s_mem[10'h0BB] <= 32'h8e7594b7;
		s_mem[10'h0BC] <= 32'h8ff6e2fb;
		s_mem[10'h0BD] <= 32'hf2122b64;
		s_mem[10'h0BE] <= 32'h8888b812;
		s_mem[10'h0BF] <= 32'h900df01c;
		s_mem[10'h0C0] <= 32'h4fad5ea0;
		s_mem[10'h0C1] <= 32'h688fc31c;
		s_mem[10'h0C2] <= 32'hd1cff191;
		s_mem[10'h0C3] <= 32'hb3a8c1ad;
		s_mem[10'h0C4] <= 32'h2f2f2218;
		s_mem[10'h0C5] <= 32'hbe0e1777;
		s_mem[10'h0C6] <= 32'hea752dfe;
		s_mem[10'h0C7] <= 32'h8b021fa1;
		s_mem[10'h0C8] <= 32'he5a0cc0f;
		s_mem[10'h0C9] <= 32'hb56f74e8;
		s_mem[10'h0CA] <= 32'h18acf3d6;
		s_mem[10'h0CB] <= 32'hce89e299;	
		s_mem[10'h0CC] <= 32'hb4a84fe0;
		s_mem[10'h0CD] <= 32'hfd13e0b7;
		s_mem[10'h0CE] <= 32'h7cc43b81;
		s_mem[10'h0CF] <= 32'hd2ada8d9;
		s_mem[10'h0D0] <= 32'h165fa266;
		s_mem[10'h0D1] <= 32'h80957705;
		s_mem[10'h0D2] <= 32'h93cc7314;
		s_mem[10'h0D3] <= 32'h211a1477;
		s_mem[10'h0D4] <= 32'he6ad2065;
		s_mem[10'h0D5] <= 32'h77b5fa86;
		s_mem[10'h0D6] <= 32'hc75442f5;
		s_mem[10'h0D7] <= 32'hfb9d35cf;
		s_mem[10'h0D8] <= 32'hebcdaf0c;
		s_mem[10'h0D9] <= 32'h7b3e89a0;
		s_mem[10'h0DA] <= 32'hd6411bd3;
		s_mem[10'h0DB] <= 32'hae1e7e49;
		s_mem[10'h0DC] <= 32'h00250e2d;
		s_mem[10'h0DD] <= 32'h2071b35e;
		s_mem[10'h0DE] <= 32'h226800bb;
		s_mem[10'h0DF] <= 32'h57b8e0af;
		s_mem[10'h0E0] <= 32'h2464369b;
		s_mem[10'h0E1] <= 32'hf009b91e;
		s_mem[10'h0E2] <= 32'h5563911d;
		s_mem[10'h0E3] <= 32'h59dfa6aa;
		s_mem[10'h0E4] <= 32'h78c14389;
		s_mem[10'h0E5] <= 32'hd95a537f;
		s_mem[10'h0E6] <= 32'h207d5ba2;
		s_mem[10'h0E7] <= 32'h02e5b9c5;
		s_mem[10'h0E8] <= 32'h83260376;
		s_mem[10'h0E9] <= 32'h6295cfa9;
		s_mem[10'h0EA] <= 32'h11c81968;
		s_mem[10'h0EB] <= 32'h4e734a41;
		s_mem[10'h0EC] <= 32'hb3472dca;
		s_mem[10'h0ED] <= 32'h7b14a94a;
		s_mem[10'h0EE] <= 32'h1b510052;
		s_mem[10'h0EF] <= 32'h9a532915;
		s_mem[10'h0F0] <= 32'hd60f573f;
		s_mem[10'h0F1] <= 32'hbc9bc6e4;
		s_mem[10'h0F2] <= 32'h2b60a476;
		s_mem[10'h0F3] <= 32'h81e67400;
		s_mem[10'h0F4] <= 32'h08ba6fb5;
		s_mem[10'h0F5] <= 32'h571be91f;
		s_mem[10'h0F6] <= 32'hf296ec6b;
		s_mem[10'h0F7] <= 32'h2a0dd915;
		s_mem[10'h0F8] <= 32'hb6636521;
		s_mem[10'h0F9] <= 32'he7b9f9b6;
		s_mem[10'h0FA] <= 32'hff34052e;
		s_mem[10'h0FB] <= 32'hc5855664;
		s_mem[10'h0FC] <= 32'h53b02d5d;
		s_mem[10'h0FD] <= 32'ha99f8fa1;
		s_mem[10'h0FE] <= 32'h08ba4799;
		s_mem[10'h0FF] <= 32'h6e85076a;
		
		// S BOX 02
		
		s_mem[10'h100] <= 32'h4b7a70e9;
		s_mem[10'h101] <= 32'hb5b32944;
		s_mem[10'h102] <= 32'hdb75092e;
		s_mem[10'h103] <= 32'hc4192623;
		s_mem[10'h104] <= 32'had6ea6b0;
		s_mem[10'h105] <= 32'h49a7df7d;
		s_mem[10'h106] <= 32'h9cee60b8;
		s_mem[10'h107] <= 32'h8fedb266;
		s_mem[10'h108] <= 32'hecaa8c71;
		s_mem[10'h109] <= 32'h699a17ff;
		s_mem[10'h10A] <= 32'h5664526c;
		s_mem[10'h10B] <= 32'hc2b19ee1;
		s_mem[10'h10C] <= 32'h193602a5;
		s_mem[10'h10D] <= 32'h75094c29;
		s_mem[10'h10E] <= 32'ha0591340;
		s_mem[10'h10F] <= 32'he4183a3e;
		s_mem[10'h110] <= 32'h3f54989a;
		s_mem[10'h111] <= 32'h5b429d65;
		s_mem[10'h112] <= 32'h6b8fe4d6;
		s_mem[10'h113] <= 32'h99f73fd6;
		s_mem[10'h114] <= 32'ha1d29c07;
		s_mem[10'h115] <= 32'hefe830f5;
		s_mem[10'h116] <= 32'h4d2d38e6;
		s_mem[10'h117] <= 32'hf0255dc1;
		s_mem[10'h118] <= 32'h4cdd2086;
		s_mem[10'h119] <= 32'h8470eb26;
		s_mem[10'h11A] <= 32'h6382e9c6;
		s_mem[10'h11B] <= 32'h021ecc5e;
		s_mem[10'h11C] <= 32'h09686b3f;
		s_mem[10'h11D] <= 32'h3ebaefc9;
		s_mem[10'h11E] <= 32'h3c971814;
		s_mem[10'h11F] <= 32'h6b6a70a1;
		s_mem[10'h120] <= 32'h687f3584;
		s_mem[10'h121] <= 32'h52a0e286;
		s_mem[10'h122] <= 32'hb79c5305;
		s_mem[10'h123] <= 32'haa500737;
		s_mem[10'h124] <= 32'h3e07841c;
		s_mem[10'h125] <= 32'h7fdeae5c;
		s_mem[10'h126] <= 32'h8e7d44ec;
		s_mem[10'h127] <= 32'h5716f2b8;
		s_mem[10'h128] <= 32'hb03ada37;
		s_mem[10'h129] <= 32'hf0500c0d;
		s_mem[10'h12A] <= 32'hf01c1f04;
		s_mem[10'h12B] <= 32'h0200b3ff;
		s_mem[10'h12C] <= 32'hae0cf51a;
		s_mem[10'h12D] <= 32'h3cb574b2;
		s_mem[10'h12E] <= 32'h25837a58;
		s_mem[10'h12F] <= 32'hdc0921bd;
		s_mem[10'h130] <= 32'hd19113f9;
		s_mem[10'h131] <= 32'h7ca92ff6;
		s_mem[10'h132] <= 32'h94324773;
		s_mem[10'h133] <= 32'h22f54701;
		s_mem[10'h134] <= 32'h3ae5e581;
		s_mem[10'h135] <= 32'h37c2dadc;
		s_mem[10'h136] <= 32'hc8b57634;
		s_mem[10'h137] <= 32'h9af3dda7;
		s_mem[10'h138] <= 32'ha9446146;
		s_mem[10'h139] <= 32'h0fd0030e;
		s_mem[10'h13A] <= 32'hecc8c73e;
		s_mem[10'h13B] <= 32'ha4751e41;
		s_mem[10'h13C] <= 32'he238cd99;
		s_mem[10'h13D] <= 32'h3bea0e2f;
		s_mem[10'h13E] <= 32'h3280bba1;
		s_mem[10'h13F] <= 32'h183eb331;
		s_mem[10'h140] <= 32'h4e548b38;
		s_mem[10'h141] <= 32'h4f6db908;
		s_mem[10'h142] <= 32'h6f420d03;
		s_mem[10'h143] <= 32'hf60a04bf;
		s_mem[10'h144] <= 32'h2cb81290;
		s_mem[10'h145] <= 32'h24977c79;
		s_mem[10'h146] <= 32'h5679b072;
		s_mem[10'h147] <= 32'hbcaf89af;
		s_mem[10'h148] <= 32'hde9a771f;
		s_mem[10'h149] <= 32'hd9930810;
		s_mem[10'h14A] <= 32'hb38bae12;
		s_mem[10'h14B] <= 32'hdccf3f2e;
		s_mem[10'h14C] <= 32'h5512721f;
		s_mem[10'h14D] <= 32'h2e6b7124;
		s_mem[10'h14E] <= 32'h501adde6;
		s_mem[10'h14F] <= 32'h9f84cd87;
		s_mem[10'h150] <= 32'h7a584718;
		s_mem[10'h151] <= 32'h7408da17;
		s_mem[10'h152] <= 32'hbc9f9abc;
		s_mem[10'h153] <= 32'he94b7d8c;
		s_mem[10'h154] <= 32'hec7aec3a;
		s_mem[10'h155] <= 32'hdb851dfa;
		s_mem[10'h156] <= 32'h63094366;
		s_mem[10'h157] <= 32'hc464c3d2;
		s_mem[10'h158] <= 32'hef1c1847;
		s_mem[10'h159] <= 32'h3215d908;
		s_mem[10'h15A] <= 32'hdd433b37;
		s_mem[10'h15B] <= 32'h24c2ba16;
		s_mem[10'h15C] <= 32'h12a14d43;
		s_mem[10'h15D] <= 32'h2a65c451;
		s_mem[10'h15E] <= 32'h50940002;
		s_mem[10'h15F] <= 32'h133ae4dd;
		s_mem[10'h160] <= 32'h71dff89e;
		s_mem[10'h161] <= 32'h10314e55;
		s_mem[10'h162] <= 32'h81ac77d6;
		s_mem[10'h163] <= 32'h5f11199b;
		s_mem[10'h164] <= 32'h043556f1;
		s_mem[10'h165] <= 32'hd7a3c76b;
		s_mem[10'h166] <= 32'h3c11183b;
		s_mem[10'h167] <= 32'h5924a509;
		s_mem[10'h168] <= 32'hf28fe6ed;
		s_mem[10'h169] <= 32'h97f1fbfa;
		s_mem[10'h16A] <= 32'h9ebabf2c;
		s_mem[10'h16B] <= 32'h1e153c6e;
		s_mem[10'h16C] <= 32'h86e34570;
		s_mem[10'h16D] <= 32'heae96fb1;
		s_mem[10'h16E] <= 32'h860e5e0a;
		s_mem[10'h16F] <= 32'h5a3e2ab3;
		s_mem[10'h170] <= 32'h771fe71c;
		s_mem[10'h171] <= 32'h4e3d06fa;
		s_mem[10'h172] <= 32'h2965dcb9;
		s_mem[10'h173] <= 32'h99e71d0f;
		s_mem[10'h174] <= 32'h803e89d6;
		s_mem[10'h175] <= 32'h5266c825;
		s_mem[10'h176] <= 32'h2e4cc978;
		s_mem[10'h177] <= 32'h9c10b36a;
		s_mem[10'h178] <= 32'hc6150eba;
		s_mem[10'h179] <= 32'h94e2ea78;
		s_mem[10'h17A] <= 32'ha5fc3c53;
		s_mem[10'h17B] <= 32'h1e0a2df4;
		s_mem[10'h17C] <= 32'hf2f74ea7;
		s_mem[10'h17D] <= 32'h361d2b3d;
		s_mem[10'h17E] <= 32'h1939260f;
		s_mem[10'h17F] <= 32'h19c27960;
		s_mem[10'h180] <= 32'h5223a708;
		s_mem[10'h181] <= 32'hf71312b6;
		s_mem[10'h182] <= 32'hebadfe6e;
		s_mem[10'h183] <= 32'heac31f66;
		s_mem[10'h184] <= 32'he3bc4595;
		s_mem[10'h185] <= 32'ha67bc883;
		s_mem[10'h186] <= 32'hb17f37d1;
		s_mem[10'h187] <= 32'h018cff28;
		s_mem[10'h188] <= 32'hc332ddef;
		s_mem[10'h189] <= 32'hbe6c5aa5;
		s_mem[10'h18A] <= 32'h65582185;
		s_mem[10'h18B] <= 32'h68ab9802;
		s_mem[10'h18C] <= 32'heecea50f;
		s_mem[10'h18D] <= 32'hdb2f953b;
		s_mem[10'h18E] <= 32'h2aef7dad;
		s_mem[10'h18F] <= 32'h5b6e2f84;
		s_mem[10'h190] <= 32'h1521b628;
		s_mem[10'h191] <= 32'h29076170;
		s_mem[10'h192] <= 32'hecdd4775;
		s_mem[10'h193] <= 32'h619f1510;
		s_mem[10'h194] <= 32'h13cca830;
		s_mem[10'h195] <= 32'heb61bd96;
		s_mem[10'h196] <= 32'h0334fe1e;
		s_mem[10'h197] <= 32'haa0363cf;
		s_mem[10'h198] <= 32'hb5735c90;
		s_mem[10'h199] <= 32'h4c70a239;
		s_mem[10'h19A] <= 32'hd59e9e0b;
		s_mem[10'h19B] <= 32'hcbaade14;
		s_mem[10'h19C] <= 32'heecc86bc;
		s_mem[10'h19D] <= 32'h60622ca7;
		s_mem[10'h19E] <= 32'h9cab5cab;
		s_mem[10'h19F] <= 32'hb2f3846e;
		s_mem[10'h1A0] <= 32'h648b1eaf;
		s_mem[10'h1A1] <= 32'h19bdf0ca;
		s_mem[10'h1A2] <= 32'ha02369b9;
		s_mem[10'h1A3] <= 32'h655abb50;
		s_mem[10'h1A4] <= 32'h40685a32;
		s_mem[10'h1A5] <= 32'h3c2ab4b3;
		s_mem[10'h1A6] <= 32'h319ee9d5;
		s_mem[10'h1A7] <= 32'hc021b8f7;
		s_mem[10'h1A8] <= 32'h9b540b19;
		s_mem[10'h1A9] <= 32'h875fa099;
		s_mem[10'h1AA] <= 32'h95f7997e;
		s_mem[10'h1AB] <= 32'h623d7da8;
		s_mem[10'h1AC] <= 32'hf837889a;
		s_mem[10'h1AD] <= 32'h97e32d77;
		s_mem[10'h1AE] <= 32'h11ed935f;
		s_mem[10'h1AF] <= 32'h16681281;
		s_mem[10'h1B0] <= 32'h0e358829;
		s_mem[10'h1B1] <= 32'hc7e61fd6;
		s_mem[10'h1B2] <= 32'h96dedfa1;
		s_mem[10'h1B3] <= 32'h7858ba99;
		s_mem[10'h1B4] <= 32'h57f584a5;
		s_mem[10'h1B5] <= 32'h1b227263;
		s_mem[10'h1B6] <= 32'h9b83c3ff;
		s_mem[10'h1B7] <= 32'h1ac24696;
		s_mem[10'h1B8] <= 32'hcdb30aeb;
		s_mem[10'h1B9] <= 32'h532e3054;
		s_mem[10'h1BA] <= 32'h8fd948e4;
		s_mem[10'h1BB] <= 32'h6dbc3128;
		s_mem[10'h1BC] <= 32'h58ebf2ef;
		s_mem[10'h1BD] <= 32'h34c6ffea;
		s_mem[10'h1BE] <= 32'hfe28ed61;
		s_mem[10'h1BF] <= 32'hee7c3c73;
		s_mem[10'h1C0] <= 32'h5d4a14d9;
		s_mem[10'h1C1] <= 32'he864b7e3;
		s_mem[10'h1C2] <= 32'h42105d14;
		s_mem[10'h1C3] <= 32'h203e13e0;
		s_mem[10'h1C4] <= 32'h45eee2b6;
		s_mem[10'h1C5] <= 32'ha3aaabea;
		s_mem[10'h1C6] <= 32'hdb6c4f15;
		s_mem[10'h1C7] <= 32'hfacb4fd0;
		s_mem[10'h1C8] <= 32'hc742f442;
		s_mem[10'h1C9] <= 32'hef6abbb5;
		s_mem[10'h1CA] <= 32'h654f3b1d;
		s_mem[10'h1CB] <= 32'h41cd2105;
		s_mem[10'h1CC] <= 32'hd81e799e;
		s_mem[10'h1CD] <= 32'h86854dc7;
		s_mem[10'h1CE] <= 32'he44b476a;
		s_mem[10'h1CF] <= 32'h3d816250;
		s_mem[10'h1D0] <= 32'hcf62a1f2;
		s_mem[10'h1D1] <= 32'h5b8d2646;
		s_mem[10'h1D2] <= 32'hfc8883a0;
		s_mem[10'h1D3] <= 32'hc1c7b6a3;
		s_mem[10'h1D4] <= 32'h7f1524c3;
		s_mem[10'h1D5] <= 32'h69cb7492;
		s_mem[10'h1D6] <= 32'h47848a0b;
		s_mem[10'h1D7] <= 32'h5692b285;
		s_mem[10'h1D8] <= 32'h095bbf00;
		s_mem[10'h1D9] <= 32'had19489d;
		s_mem[10'h1DA] <= 32'h1462b174;
		s_mem[10'h1DB] <= 32'h23820e00;
		s_mem[10'h1DC] <= 32'h58428d2a;
		s_mem[10'h1DD] <= 32'h0c55f5ea;
		s_mem[10'h1DE] <= 32'h1dadf43e;
		s_mem[10'h1DF] <= 32'h233f7061;
		s_mem[10'h1E0] <= 32'h3372f092;
		s_mem[10'h1E1] <= 32'h8d937e41;
		s_mem[10'h1E2] <= 32'hd65fecf1;
		s_mem[10'h1E3] <= 32'h6c223bdb;
		s_mem[10'h1E4] <= 32'h7cde3759;
		s_mem[10'h1E5] <= 32'hcbee7460;
		s_mem[10'h1E6] <= 32'h4085f2a7;
		s_mem[10'h1E7] <= 32'hce77326e;
		s_mem[10'h1E8] <= 32'ha6078084;
		s_mem[10'h1E9] <= 32'h19f8509e;
		s_mem[10'h1EA] <= 32'he8efd855;
		s_mem[10'h1EB] <= 32'h61d99735;
		s_mem[10'h1EC] <= 32'ha969a7aa;
		s_mem[10'h1ED] <= 32'hc50c06c2;
		s_mem[10'h1EE] <= 32'h5a04abfc;
		s_mem[10'h1EF] <= 32'h800bcadc;
		s_mem[10'h1F0] <= 32'h9e447a2e;
		s_mem[10'h1F1] <= 32'hc3453484;
		s_mem[10'h1F2] <= 32'hfdd56705;
		s_mem[10'h1F3] <= 32'h0e1e9ec9;
		s_mem[10'h1F4] <= 32'hdb73dbd3;
		s_mem[10'h1F5] <= 32'h105588cd;
		s_mem[10'h1F6] <= 32'h675fda79;
		s_mem[10'h1F7] <= 32'he3674340;
		s_mem[10'h1F8] <= 32'hc5c43465;
		s_mem[10'h1F9] <= 32'h713e38d8;
		s_mem[10'h1FA] <= 32'h3d28f89e;
		s_mem[10'h1FB] <= 32'hf16dff20;
		s_mem[10'h1FC] <= 32'h153e21e7;
		s_mem[10'h1FD] <= 32'h8fb03d4a;
		s_mem[10'h1FE] <= 32'he6e39f2b;
		s_mem[10'h1FF] <= 32'hdb83adf7;
		
		// S BOX 03
		
		s_mem[10'h200] <= 32'he93d5a68;
		s_mem[10'h201] <= 32'h948140f7;
		s_mem[10'h202] <= 32'hf64c261c;
		s_mem[10'h203] <= 32'h94692934;
		s_mem[10'h204] <= 32'h411520f7;
		s_mem[10'h205] <= 32'h7602d4f7;
		s_mem[10'h206] <= 32'hbcf46b2e;
		s_mem[10'h207] <= 32'hd4a20068;
		s_mem[10'h208] <= 32'hd4082471;
		s_mem[10'h209] <= 32'h3320f46a;
		s_mem[10'h20A] <= 32'h43b7d4b7;
		s_mem[10'h20B] <= 32'h500061af;
		s_mem[10'h20C] <= 32'h1e39f62e;
		s_mem[10'h20D] <= 32'h97244546;
		s_mem[10'h20E] <= 32'h14214f74;
		s_mem[10'h20F] <= 32'hbf8b8840;
		s_mem[10'h210] <= 32'h4d95fc1d;
		s_mem[10'h211] <= 32'h96b591af;
		s_mem[10'h212] <= 32'h70f4ddd3;
		s_mem[10'h213] <= 32'h66a02f45;
		s_mem[10'h214] <= 32'hbfbc09ec;
		s_mem[10'h215] <= 32'h03bd9785;
		s_mem[10'h216] <= 32'h7fac6dd0;
		s_mem[10'h217] <= 32'h31cb8504;
		s_mem[10'h218] <= 32'h96eb27b3;
		s_mem[10'h219] <= 32'h55fd3941;
		s_mem[10'h21A] <= 32'hda2547e6;
		s_mem[10'h21B] <= 32'habca0a9a;
		s_mem[10'h21C] <= 32'h28507825;
		s_mem[10'h21D] <= 32'h530429f4;
		s_mem[10'h21E] <= 32'h0a2c86da;
		s_mem[10'h21F] <= 32'he9b66dfb;
		s_mem[10'h220] <= 32'h68dc1462;
		s_mem[10'h221] <= 32'hd7486900;
		s_mem[10'h222] <= 32'h680ec0a4;
		s_mem[10'h223] <= 32'h27a18dee;
		s_mem[10'h224] <= 32'h4f3ffea2;
		s_mem[10'h225] <= 32'he887ad8c;
		s_mem[10'h226] <= 32'hb58ce006;
		s_mem[10'h227] <= 32'h7af4d6b6;
		s_mem[10'h228] <= 32'haace1e7c;
		s_mem[10'h229] <= 32'hd3375fec;
		s_mem[10'h22A] <= 32'hce78a399;
		s_mem[10'h22B] <= 32'h406b2a42;
		s_mem[10'h22C] <= 32'h20fe9e35;
		s_mem[10'h22D] <= 32'hd9f385b9;
		s_mem[10'h22E] <= 32'hee39d7ab;
		s_mem[10'h22F] <= 32'h3b124e8b;
		s_mem[10'h230] <= 32'h1dc9faf7;
		s_mem[10'h231] <= 32'h4b6d1856;
		s_mem[10'h232] <= 32'h26a36631;
		s_mem[10'h233] <= 32'heae397b2;
		s_mem[10'h234] <= 32'h3a6efa74;
		s_mem[10'h235] <= 32'hdd5b4332;
		s_mem[10'h236] <= 32'h6841e7f7;
		s_mem[10'h237] <= 32'hca7820fb;
		s_mem[10'h238] <= 32'hfb0af54e;
		s_mem[10'h239] <= 32'hd8feb397;
		s_mem[10'h23A] <= 32'h454056ac;
		s_mem[10'h23B] <= 32'hba489527;
		s_mem[10'h23C] <= 32'h55533a3a;
		s_mem[10'h23D] <= 32'h20838d87;
		s_mem[10'h23E] <= 32'hfe6ba9b7;
		s_mem[10'h23F] <= 32'hd096954b;
		s_mem[10'h240] <= 32'h55a867bc;
		s_mem[10'h241] <= 32'ha1159a58;
		s_mem[10'h242] <= 32'hcca92963;
		s_mem[10'h243] <= 32'h99e1db33;
		s_mem[10'h244] <= 32'ha62a4a56;
		s_mem[10'h245] <= 32'h3f3125f9;
		s_mem[10'h246] <= 32'h5ef47e1c;
		s_mem[10'h247] <= 32'h9029317c;
		s_mem[10'h248] <= 32'hfdf8e802;
		s_mem[10'h249] <= 32'h04272f70;
		s_mem[10'h24A] <= 32'h80bb155c;
		s_mem[10'h24B] <= 32'h05282ce3;
		s_mem[10'h24C] <= 32'h95c11548;
		s_mem[10'h24D] <= 32'he4c66d22;
		s_mem[10'h24E] <= 32'h48c1133f;
		s_mem[10'h24F] <= 32'hc70f86dc;
		s_mem[10'h250] <= 32'h07f9c9ee;
		s_mem[10'h251] <= 32'h41041f0f;
		s_mem[10'h252] <= 32'h404779a4;
		s_mem[10'h253] <= 32'h5d886e17;
		s_mem[10'h254] <= 32'h325f51eb;
		s_mem[10'h255] <= 32'hd59bc0d1;
		s_mem[10'h256] <= 32'hf2bcc18f;
		s_mem[10'h257] <= 32'h41113564;
		s_mem[10'h258] <= 32'h257b7834;
		s_mem[10'h259] <= 32'h602a9c60;
		s_mem[10'h25A] <= 32'hdff8e8a3;
		s_mem[10'h25B] <= 32'h1f636c1b;
		s_mem[10'h25C] <= 32'h0e12b4c2;
		s_mem[10'h25D] <= 32'h02e1329e;
		s_mem[10'h25E] <= 32'haf664fd1;
		s_mem[10'h25F] <= 32'hcad18115;
		s_mem[10'h260] <= 32'h6b2395e0;
		s_mem[10'h261] <= 32'h333e92e1;
		s_mem[10'h262] <= 32'h3b240b62;
		s_mem[10'h263] <= 32'heebeb922;
		s_mem[10'h264] <= 32'h85b2a20e;
		s_mem[10'h265] <= 32'he6ba0d99;
		s_mem[10'h266] <= 32'hde720c8c;
		s_mem[10'h267] <= 32'h2da2f728;
		s_mem[10'h268] <= 32'hd0127845;
		s_mem[10'h269] <= 32'h95b794fd;
		s_mem[10'h26A] <= 32'h647d0862;
		s_mem[10'h26B] <= 32'he7ccf5f0;
		s_mem[10'h26C] <= 32'h5449a36f;
		s_mem[10'h26D] <= 32'h877d48fa;
		s_mem[10'h26E] <= 32'hc39dfd27;
		s_mem[10'h26F] <= 32'hf33e8d1e;
		s_mem[10'h270] <= 32'h0a476341;
		s_mem[10'h271] <= 32'h992eff74;
		s_mem[10'h272] <= 32'h3a6f6eab;
		s_mem[10'h273] <= 32'hf4f8fd37;
		s_mem[10'h274] <= 32'ha812dc60;
		s_mem[10'h275] <= 32'ha1ebddf8;
		s_mem[10'h276] <= 32'h991be14c;
		s_mem[10'h277] <= 32'hdb6e6b0d;
		s_mem[10'h278] <= 32'hc67b5510;
		s_mem[10'h279] <= 32'h6d672c37;
		s_mem[10'h27A] <= 32'h2765d43b;
		s_mem[10'h27B] <= 32'hdcd0e804;
		s_mem[10'h27C] <= 32'hf1290dc7;
		s_mem[10'h27D] <= 32'hcc00ffa3;
		s_mem[10'h27E] <= 32'hb5390f92;
		s_mem[10'h27F] <= 32'h690fed0b;
		s_mem[10'h280] <= 32'h667b9ffb;
		s_mem[10'h281] <= 32'hcedb7d9c;
		s_mem[10'h282] <= 32'ha091cf0b;
		s_mem[10'h283] <= 32'hd9155ea3;
		s_mem[10'h284] <= 32'hbb132f88;
		s_mem[10'h285] <= 32'h515bad24;
		s_mem[10'h286] <= 32'h7b9479bf;
		s_mem[10'h287] <= 32'h763bd6eb;
		s_mem[10'h288] <= 32'h37392eb3;
		s_mem[10'h289] <= 32'hcc115979;
		s_mem[10'h28A] <= 32'h8026e297;
		s_mem[10'h28B] <= 32'hf42e312d;
		s_mem[10'h28C] <= 32'h6842ada7;
		s_mem[10'h28D] <= 32'hc66a2b3b;
		s_mem[10'h28E] <= 32'h12754ccc;
		s_mem[10'h28F] <= 32'h782ef11c;
		s_mem[10'h290] <= 32'h6a124237;
		s_mem[10'h291] <= 32'hb79251e7;
		s_mem[10'h292] <= 32'h06a1bbe6;
		s_mem[10'h293] <= 32'h4bfb6350;
		s_mem[10'h294] <= 32'h1a6b1018;
		s_mem[10'h295] <= 32'h11caedfa;
		s_mem[10'h296] <= 32'h3d25bdd8;
		s_mem[10'h297] <= 32'he2e1c3c9;
		s_mem[10'h298] <= 32'h44421659;
		s_mem[10'h299] <= 32'h0a121386;
		s_mem[10'h29A] <= 32'hd90cec6e;
		s_mem[10'h29B] <= 32'hd5abea2a;
		s_mem[10'h29C] <= 32'h64af674e;
		s_mem[10'h29D] <= 32'hda86a85f;
		s_mem[10'h29E] <= 32'hbebfe988;
		s_mem[10'h29F] <= 32'h64e4c3fe;
		s_mem[10'h2A0] <= 32'h9dbc8057;
		s_mem[10'h2A1] <= 32'hf0f7c086;
		s_mem[10'h2A2] <= 32'h60787bf8;
		s_mem[10'h2A3] <= 32'h6003604d;
		s_mem[10'h2A4] <= 32'hd1fd8346;
		s_mem[10'h2A5] <= 32'hf6381fb0;
		s_mem[10'h2A6] <= 32'h7745ae04;
		s_mem[10'h2A7] <= 32'hd736fccc;
		s_mem[10'h2A8] <= 32'h83426b33;
		s_mem[10'h2A9] <= 32'hf01eab71;
		s_mem[10'h2AA] <= 32'hb0804187;
		s_mem[10'h2AB] <= 32'h3c005e5f;
		s_mem[10'h2AC] <= 32'h77a057be;
		s_mem[10'h2AD] <= 32'hbde8ae24;
		s_mem[10'h2AE] <= 32'h55464299;
		s_mem[10'h2AF] <= 32'hbf582e61;
		s_mem[10'h2B0] <= 32'h4e58f48f;
		s_mem[10'h2B1] <= 32'hf2ddfda2;
		s_mem[10'h2B2] <= 32'hf474ef38;
		s_mem[10'h2B3] <= 32'h8789bdc2;
		s_mem[10'h2B4] <= 32'h5366f9c3;
		s_mem[10'h2B5] <= 32'hc8b38e74;
		s_mem[10'h2B6] <= 32'hb475f255;
		s_mem[10'h2B7] <= 32'h46fcd9b9;
		s_mem[10'h2B8] <= 32'h7aeb2661;
		s_mem[10'h2B9] <= 32'h8b1ddf84;
		s_mem[10'h2BA] <= 32'h846a0e79;
		s_mem[10'h2BB] <= 32'h915f95e2;
		s_mem[10'h2BC] <= 32'h466e598e;
		s_mem[10'h2BD] <= 32'h20b45770;
		s_mem[10'h2BE] <= 32'h8cd55591;
		s_mem[10'h2BF] <= 32'hc902de4c;
		s_mem[10'h2C0] <= 32'hb90bace1;
		s_mem[10'h2C1] <= 32'hbb8205d0;
		s_mem[10'h2C2] <= 32'h11a86248;
		s_mem[10'h2C3] <= 32'h7574a99e;
		s_mem[10'h2C4] <= 32'hb77f19b6;
		s_mem[10'h2C5] <= 32'he0a9dc09;
		s_mem[10'h2C6] <= 32'h662d09a1;
		s_mem[10'h2C7] <= 32'hc4324633;
		s_mem[10'h2C8] <= 32'he85a1f02;
		s_mem[10'h2C9] <= 32'h09f0be8c;
		s_mem[10'h2CA] <= 32'h4a99a025;
		s_mem[10'h2CB] <= 32'h1d6efe10;
		s_mem[10'h2CC] <= 32'h1ab93d1d;
		s_mem[10'h2CD] <= 32'h0ba5a4df;
		s_mem[10'h2CE] <= 32'ha186f20f;
		s_mem[10'h2CF] <= 32'h2868f169;
		s_mem[10'h2D0] <= 32'hdcb7da83;
		s_mem[10'h2D1] <= 32'h573906fe;
		s_mem[10'h2D2] <= 32'ha1e2ce9b;
		s_mem[10'h2D3] <= 32'h4fcd7f52;
		s_mem[10'h2D4] <= 32'h50115e01;
		s_mem[10'h2D5] <= 32'ha70683fa;
		s_mem[10'h2D6] <= 32'ha002b5c4;
		s_mem[10'h2D7] <= 32'h0de6d027;
		s_mem[10'h2D8] <= 32'h9af88c27;
		s_mem[10'h2D9] <= 32'h773f8641;
		s_mem[10'h2DA] <= 32'hc3604c06;
		s_mem[10'h2DB] <= 32'h61a806b5;
		s_mem[10'h2DC] <= 32'hf0177a28;
		s_mem[10'h2DD] <= 32'hc0f586e0;
		s_mem[10'h2DE] <= 32'h006058aa;
		s_mem[10'h2DF] <= 32'h30dc7d62;
		s_mem[10'h2E0] <= 32'h11e69ed7;
		s_mem[10'h2E1] <= 32'h2338ea63;
		s_mem[10'h2E2] <= 32'h53c2dd94;
		s_mem[10'h2E3] <= 32'hc2c21634;
		s_mem[10'h2E4] <= 32'hbbcbee56;
		s_mem[10'h2E5] <= 32'h90bcb6de;
		s_mem[10'h2E6] <= 32'hebfc7da1;
		s_mem[10'h2E7] <= 32'hce591d76;
		s_mem[10'h2E8] <= 32'h6f05e409;
		s_mem[10'h2E9] <= 32'h4b7c0188;
		s_mem[10'h2EA] <= 32'h39720a3d;
		s_mem[10'h2EB] <= 32'h7c927c24;
		s_mem[10'h2EC] <= 32'h86e3725f;
		s_mem[10'h2ED] <= 32'h724d9db9;
		s_mem[10'h2EE] <= 32'h1ac15bb4;
		s_mem[10'h2EF] <= 32'hd39eb8fc;
		s_mem[10'h2F0] <= 32'hed545578;
		s_mem[10'h2F1] <= 32'h08fca5b5;
		s_mem[10'h2F2] <= 32'hd83d7cd3;
		s_mem[10'h2F3] <= 32'h4dad0fc4;
		s_mem[10'h2F4] <= 32'h1e50ef5e;
		s_mem[10'h2F5] <= 32'hb161e6f8;
		s_mem[10'h2F6] <= 32'ha28514d9;
		s_mem[10'h2F7] <= 32'h6c51133c;
		s_mem[10'h2F8] <= 32'h6fd5c7e7;
		s_mem[10'h2F9] <= 32'h56e14ec4;
		s_mem[10'h2FA] <= 32'h362abfce;
		s_mem[10'h2FB] <= 32'hddc6c837;
		s_mem[10'h2FC] <= 32'hd79a3234;
		s_mem[10'h2FD] <= 32'h92638212;
		s_mem[10'h2FE] <= 32'h670efa8e;
		s_mem[10'h2FF] <= 32'h406000e0;
		
		// S BOX 04
		
		s_mem[10'h300] <= 32'h3a39ce37;
		s_mem[10'h301] <= 32'hd3faf5cf;
		s_mem[10'h302] <= 32'habc27737;
		s_mem[10'h303] <= 32'h5ac52d1b;
		s_mem[10'h304] <= 32'h5cb0679e;
		s_mem[10'h305] <= 32'h4fa33742;
		s_mem[10'h306] <= 32'hd3822740;
		s_mem[10'h307] <= 32'h99bc9bbe;
		s_mem[10'h308] <= 32'hd5118e9d;
		s_mem[10'h309] <= 32'hbf0f7315;
		s_mem[10'h30A] <= 32'hd62d1c7e;
		s_mem[10'h30B] <= 32'hc700c47b;
		s_mem[10'h30C] <= 32'hb78c1b6b;
		s_mem[10'h30D] <= 32'h21a19045;
		s_mem[10'h30E] <= 32'hb26eb1be;
		s_mem[10'h30F] <= 32'h6a366eb4;
		s_mem[10'h310] <= 32'h5748ab2f;
		s_mem[10'h311] <= 32'hbc946e79;
		s_mem[10'h312] <= 32'hc6a376d2;
		s_mem[10'h313] <= 32'h6549c2c8;
		s_mem[10'h314] <= 32'h530ff8ee;
		s_mem[10'h315] <= 32'h468dde7d;
		s_mem[10'h316] <= 32'hd5730a1d;
		s_mem[10'h317] <= 32'h4cd04dc6;
		s_mem[10'h318] <= 32'h2939bbdb;
		s_mem[10'h319] <= 32'ha9ba4650;
		s_mem[10'h31A] <= 32'hac9526e8;
		s_mem[10'h31B] <= 32'hbe5ee304;
		s_mem[10'h31C] <= 32'ha1fad5f0;
		s_mem[10'h31D] <= 32'h6a2d519a;
		s_mem[10'h31E] <= 32'h63ef8ce2;
		s_mem[10'h31F] <= 32'h9a86ee22;
		s_mem[10'h320] <= 32'hc089c2b8;
		s_mem[10'h321] <= 32'h43242ef6;
		s_mem[10'h322] <= 32'ha51e03aa;
		s_mem[10'h323] <= 32'h9cf2d0a4;
		s_mem[10'h324] <= 32'h83c061ba;
		s_mem[10'h325] <= 32'h9be96a4d;
		s_mem[10'h326] <= 32'h8fe51550;
		s_mem[10'h327] <= 32'hba645bd6;
		s_mem[10'h328] <= 32'h2826a2f9;
		s_mem[10'h329] <= 32'ha73a3ae1;
		s_mem[10'h32A] <= 32'h4ba99586;
		s_mem[10'h32B] <= 32'hef5562e9;
		s_mem[10'h32C] <= 32'hc72fefd3;
		s_mem[10'h32D] <= 32'hf752f7da;
		s_mem[10'h32E] <= 32'h3f046f69;
		s_mem[10'h32F] <= 32'h77fa0a59;
		s_mem[10'h330] <= 32'h80e4a915;
		s_mem[10'h331] <= 32'h87b08601;
		s_mem[10'h332] <= 32'h9b09e6ad;
		s_mem[10'h333] <= 32'h3b3ee593;
		s_mem[10'h334] <= 32'he990fd5a;
		s_mem[10'h335] <= 32'h9e34d797;
		s_mem[10'h336] <= 32'h2cf0b7d9;
		s_mem[10'h337] <= 32'h022b8b51;
		s_mem[10'h338] <= 32'h96d5ac3a;
		s_mem[10'h339] <= 32'h017da67d;
		s_mem[10'h33A] <= 32'hd1cf3ed6;
		s_mem[10'h33B] <= 32'h7c7d2d28;
		s_mem[10'h33C] <= 32'h1f9f25cf;
		s_mem[10'h33D] <= 32'hadf2b89b;
		s_mem[10'h33E] <= 32'h5ad6b472;
		s_mem[10'h33F] <= 32'h5a88f54c;
		s_mem[10'h340] <= 32'he029ac71;
		s_mem[10'h341] <= 32'he019a5e6;
		s_mem[10'h342] <= 32'h47b0acfd;
		s_mem[10'h343] <= 32'hed93fa9b;
		s_mem[10'h344] <= 32'he8d3c48d;
		s_mem[10'h345] <= 32'h283b57cc;
		s_mem[10'h346] <= 32'hf8d56629;
		s_mem[10'h347] <= 32'h79132e28;
		s_mem[10'h348] <= 32'h785f0191;
		s_mem[10'h349] <= 32'hed756055;
		s_mem[10'h34A] <= 32'hf7960e44;
		s_mem[10'h34B] <= 32'he3d35e8c;
		s_mem[10'h34C] <= 32'h15056dd4;
		s_mem[10'h34D] <= 32'h88f46dba;
		s_mem[10'h34E] <= 32'h03a16125;
		s_mem[10'h34F] <= 32'h0564f0bd;
		s_mem[10'h350] <= 32'hc3eb9e15;
		s_mem[10'h351] <= 32'h3c9057a2;
		s_mem[10'h352] <= 32'h97271aec;
		s_mem[10'h353] <= 32'ha93a072a;
		s_mem[10'h354] <= 32'h1b3f6d9b;
		s_mem[10'h355] <= 32'h1e6321f5;
		s_mem[10'h356] <= 32'hf59c66fb;
		s_mem[10'h357] <= 32'h26dcf319;
		s_mem[10'h358] <= 32'h7533d928;
		s_mem[10'h359] <= 32'hb155fdf5;
		s_mem[10'h35A] <= 32'h03563482;
		s_mem[10'h35B] <= 32'h8aba3cbb;
		s_mem[10'h35C] <= 32'h28517711;
		s_mem[10'h35D] <= 32'hc20ad9f8;
		s_mem[10'h35E] <= 32'habcc5167;
		s_mem[10'h35F] <= 32'hccad925f;
		s_mem[10'h360] <= 32'h4de81751;
		s_mem[10'h361] <= 32'h3830dc8e;
		s_mem[10'h362] <= 32'h379d5862;
		s_mem[10'h363] <= 32'h9320f991;
		s_mem[10'h364] <= 32'hea7a90c2;
		s_mem[10'h365] <= 32'hfb3e7bce;
		s_mem[10'h366] <= 32'h5121ce64;
		s_mem[10'h367] <= 32'h774fbe32;
		s_mem[10'h368] <= 32'ha8b6e37e;
		s_mem[10'h369] <= 32'hc3293d46;
		s_mem[10'h36A] <= 32'h48de5369;
		s_mem[10'h36B] <= 32'h6413e680;
		s_mem[10'h36C] <= 32'ha2ae0810;
		s_mem[10'h36D] <= 32'hdd6db224;
		s_mem[10'h36E] <= 32'h69852dfd;
		s_mem[10'h36F] <= 32'h09072166;
		s_mem[10'h370] <= 32'hb39a460a;
		s_mem[10'h371] <= 32'h6445c0dd;
		s_mem[10'h372] <= 32'h586cdecf;
		s_mem[10'h373] <= 32'h1c20c8ae;
		s_mem[10'h374] <= 32'h5bbef7dd;
		s_mem[10'h375] <= 32'h1b588d40;
		s_mem[10'h376] <= 32'hccd2017f;
		s_mem[10'h377] <= 32'h6bb4e3bb;
		s_mem[10'h378] <= 32'hdda26a7e;
		s_mem[10'h379] <= 32'h3a59ff45;
		s_mem[10'h37A] <= 32'h3e350a44;
		s_mem[10'h37B] <= 32'hbcb4cdd5;
		s_mem[10'h37C] <= 32'h72eacea8;
		s_mem[10'h37D] <= 32'hfa6484bb;
		s_mem[10'h37E] <= 32'h8d6612ae;
		s_mem[10'h37F] <= 32'hbf3c6f47;
		s_mem[10'h380] <= 32'hd29be463;
		s_mem[10'h381] <= 32'h542f5d9e;
		s_mem[10'h382] <= 32'haec2771b;
		s_mem[10'h383] <= 32'hf64e6370;
		s_mem[10'h384] <= 32'h740e0d8d;
		s_mem[10'h385] <= 32'he75b1357;
		s_mem[10'h386] <= 32'hf8721671;
		s_mem[10'h387] <= 32'haf537d5d;
		s_mem[10'h388] <= 32'h4040cb08;
		s_mem[10'h389] <= 32'h4eb4e2cc;
		s_mem[10'h38A] <= 32'h34d2466a;
		s_mem[10'h38B] <= 32'h0115af84;
		s_mem[10'h38C] <= 32'he1b00428;
		s_mem[10'h38D] <= 32'h95983a1d;
		s_mem[10'h38E] <= 32'h06b89fb4;
		s_mem[10'h38F] <= 32'hce6ea048;
		s_mem[10'h390] <= 32'h6f3f3b82;
		s_mem[10'h391] <= 32'h3520ab82;
		s_mem[10'h392] <= 32'h011a1d4b;
		s_mem[10'h393] <= 32'h277227f8;
		s_mem[10'h394] <= 32'h611560b1;
		s_mem[10'h395] <= 32'he7933fdc;
		s_mem[10'h396] <= 32'hbb3a792b;
		s_mem[10'h397] <= 32'h344525bd;
		s_mem[10'h398] <= 32'ha08839e1;
		s_mem[10'h399] <= 32'h51ce794b;
		s_mem[10'h39A] <= 32'h2f32c9b7;
		s_mem[10'h39B] <= 32'ha01fbac9;
		s_mem[10'h39C] <= 32'he01cc87e;
		s_mem[10'h39D] <= 32'hbcc7d1f6;
		s_mem[10'h39E] <= 32'hcf0111c3;
		s_mem[10'h39F] <= 32'ha1e8aac7;
		s_mem[10'h3A0] <= 32'h1a908749;
		s_mem[10'h3A1] <= 32'hd44fbd9a;
		s_mem[10'h3A2] <= 32'hd0dadecb;
		s_mem[10'h3A3] <= 32'hd50ada38;
		s_mem[10'h3A4] <= 32'h0339c32a;
		s_mem[10'h3A5] <= 32'hc6913667;
		s_mem[10'h3A6] <= 32'h8df9317c;
		s_mem[10'h3A7] <= 32'he0b12b4f;
		s_mem[10'h3A8] <= 32'hf79e59b7;
		s_mem[10'h3A9] <= 32'h43f5bb3a;
		s_mem[10'h3AA] <= 32'hf2d519ff;
		s_mem[10'h3AB] <= 32'h27d9459c;
		s_mem[10'h3AC] <= 32'hbf97222c;
		s_mem[10'h3AD] <= 32'h15e6fc2a;
		s_mem[10'h3AE] <= 32'h0f91fc71;
		s_mem[10'h3AF] <= 32'h9b941525;
		s_mem[10'h3B0] <= 32'hfae59361;
		s_mem[10'h3B1] <= 32'hceb69ceb;
		s_mem[10'h3B2] <= 32'hc2a86459;
		s_mem[10'h3B3] <= 32'h12baa8d1;
		s_mem[10'h3B4] <= 32'hb6c1075e;
		s_mem[10'h3B5] <= 32'he3056a0c;
		s_mem[10'h3B6] <= 32'h10d25065;
		s_mem[10'h3B7] <= 32'hcb03a442;
		s_mem[10'h3B8] <= 32'he0ec6e0e;
		s_mem[10'h3B9] <= 32'h1698db3b;
		s_mem[10'h3BA] <= 32'h4c98a0be;
		s_mem[10'h3BB] <= 32'h3278e964;
		s_mem[10'h3BC] <= 32'h9f1f9532;
		s_mem[10'h3BD] <= 32'he0d392df;
		s_mem[10'h3BE] <= 32'hd3a0342b;
		s_mem[10'h3BF] <= 32'h8971f21e;
		s_mem[10'h3C0] <= 32'h1b0a7441;
		s_mem[10'h3C1] <= 32'h4ba3348c;
		s_mem[10'h3C2] <= 32'hc5be7120;
		s_mem[10'h3C3] <= 32'hc37632d8;
		s_mem[10'h3C4] <= 32'hdf359f8d;
		s_mem[10'h3C5] <= 32'h9b992f2e;
		s_mem[10'h3C6] <= 32'he60b6f47;
		s_mem[10'h3C7] <= 32'h0fe3f11d;
		s_mem[10'h3C8] <= 32'he54cda54;
		s_mem[10'h3C9] <= 32'h1edad891;
		s_mem[10'h3CA] <= 32'hce6279cf;
		s_mem[10'h3CB] <= 32'hcd3e7e6f;
		s_mem[10'h3CC] <= 32'h1618b166;
		s_mem[10'h3CD] <= 32'hfd2c1d05;
		s_mem[10'h3CE] <= 32'h848fd2c5;
		s_mem[10'h3CF] <= 32'hf6fb2299;
		s_mem[10'h3D0] <= 32'hf523f357;
		s_mem[10'h3D1] <= 32'ha6327623;
		s_mem[10'h3D2] <= 32'h93a83531;
		s_mem[10'h3D3] <= 32'h56cccd02;
		s_mem[10'h3D4] <= 32'hacf08162;
		s_mem[10'h3D5] <= 32'h5a75ebb5;
		s_mem[10'h3D6] <= 32'h6e163697;
		s_mem[10'h3D7] <= 32'h88d273cc;
		s_mem[10'h3D8] <= 32'hde966292;
		s_mem[10'h3D9] <= 32'h81b949d0;
		s_mem[10'h3DA] <= 32'h4c50901b;
		s_mem[10'h3DB] <= 32'h71c65614;
		s_mem[10'h3DC] <= 32'he6c6c7bd;
		s_mem[10'h3DD] <= 32'h327a140a;
		s_mem[10'h3DE] <= 32'h45e1d006;
		s_mem[10'h3DF] <= 32'hc3f27b9a;
		s_mem[10'h3E0] <= 32'hc9aa53fd;
		s_mem[10'h3E1] <= 32'h62a80f00;
		s_mem[10'h3E2] <= 32'hbb25bfe2;
		s_mem[10'h3E3] <= 32'h35bdd2f6;
		s_mem[10'h3E4] <= 32'h71126905;
		s_mem[10'h3E5] <= 32'hb2040222;
		s_mem[10'h3E6] <= 32'hb6cbcf7c;
		s_mem[10'h3E7] <= 32'hcd769c2b;
		s_mem[10'h3E8] <= 32'h53113ec0;
		s_mem[10'h3E9] <= 32'h1640e3d3;
		s_mem[10'h3EA] <= 32'h38abbd60;
		s_mem[10'h3EB] <= 32'h2547adf0;
		s_mem[10'h3EC] <= 32'hba38209c;
		s_mem[10'h3ED] <= 32'hf746ce76;
		s_mem[10'h3EE] <= 32'h77afa1c5;
		s_mem[10'h3EF] <= 32'h20756060;
		s_mem[10'h3F0] <= 32'h85cbfe4e;
		s_mem[10'h3F1] <= 32'h8ae88dd8;
		s_mem[10'h3F2] <= 32'h7aaaf9b0;
		s_mem[10'h3F3] <= 32'h4cf9aa7e;
		s_mem[10'h3F4] <= 32'h1948c25c;
		s_mem[10'h3F5] <= 32'h02fb8a8c;
		s_mem[10'h3F6] <= 32'h01c36ae4;
		s_mem[10'h3F7] <= 32'hd6ebe1f9;
		s_mem[10'h3F8] <= 32'h90d4f869;
		s_mem[10'h3F9] <= 32'ha65cdea0;
		s_mem[10'h3FA] <= 32'h3f09252d;
		s_mem[10'h3FB] <= 32'hc208e69f;
		s_mem[10'h3FC] <= 32'hb74e6132;
		s_mem[10'h3FD] <= 32'hce77e25b;
		s_mem[10'h3FE] <= 32'h578fdfe3;
		s_mem[10'h3FF] <= 32'h3ac372e6;

	end
end

// ------------------------------------  Feistal Function ---------------------------------------

function [31:0] Feistal;
input [31:0] s0a,s1b,s2c,s3d;
	reg [31:0] s1,s2;
		begin
			s1 = s0a + s1b;
			s2 = s1 ^ s2c;
			Feistal = s2 + s3d;
		end
endfunction

endmodule

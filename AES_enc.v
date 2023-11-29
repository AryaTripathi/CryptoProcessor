`timescale 1ns / 1ps

module AES_enc(clk, data_in, key, data_out);
    input clk;
	 input [127:0] data_in;
	 input [127:0] key;
    output [127:0] data_out;

aescipher u1(clk, data_in, key, data_out);

endmodule

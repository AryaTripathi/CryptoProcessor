module Mid_Round(in,key,out);
input [127:0] in;
output [127:0] out;
input [127:0] key;
wire [127:0] afterSubBytes;
wire [127:0] afterShiftRows;
wire [127:0] afterMixColumns;
wire [127:0] afterAddroundKey;

Inverse_shiftrow r(in,afterShiftRows);
Inverse_subbytes s(afterShiftRows,afterSubBytes);
addRoundKey b(afterSubBytes,afterAddroundKey,key);
Inverse_MixColumns m(afterAddroundKey,out);
		
endmodule

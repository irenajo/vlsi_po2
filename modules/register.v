module register #(
    parameter DATA_WIDTH = 16,
    parameter HIGH = DATA_WIDTH-1
) (
    clk, rst_n, cl, ld, in, inc, dec, sr, ir, sl, il, out
);

input clk, rst_n, cl, ld, inc, dec, sr, ir, sl, il;
input [HIGH:0] in;
output [HIGH:0] out;

reg [HIGH:0] out_reg, out_next;
assign out = out_reg;

always @(posedge clk, negedge rst_n)
    if (!rst_n)
        out_reg <= 4'h0;
    else
        out_reg <= out_next;
    
always @(cl,ld,inc,dec,sr,sl, in, ir, il) begin
    out_next = out_reg;
    if(cl) out_next = 4'h0;
    else if(ld) out_next = in;
    else if(inc) out_next = out_reg + 1'b1;
    else if(dec) out_next = out_reg - 1'b1;
    else if(sr) out_next = {ir, out_reg[HIGH:1]}; 
    else if(sl) out_next = {out_reg[HIGH-1:0], il};
end
endmodule
module top #(
    parameter DIVISOR = 50_000_000,
    parameter FILE_NAME = "mem_init.mif",
    parameter ADDR_WIDTH = 6,
    parameter DATA_WIDTH = 16
) (
    input clk,
    input rst_n,
    input [1:0] kbd,
    input [2:0] btn,
    input [8:0] sw,
    output [13:0] mnt,
    output [9:0] led,
    output [27:0] hex
);
    
    wire slow_clk;
    clk_div #(.DIVISOR(DIVISOR)) CLK_DIV (.clk(clk), .rst_n(rst_n), .out(slow_clk));

    wire we;
    wire [ADDR_WIDTH - 1:0] addr;
    wire [DATA_WIDTH - 1:0] data;
    wire [DATA_WIDTH - 1:0] mem;
    memory #(.FILE_NAME(FILE_NAME), .ADDR_WIDTH(ADDR_WIDTH), .DATA_WIDTH(DATA_WIDTH)) MEMORY (.clk(slow_clk), .we(we), .addr(addr), .data(data), .out(mem));

    wire control, status;
    wire [DATA_WIDTH - 1:0] in;
    wire [ADDR_WIDTH - 1:0] pc, sp;
    wire [DATA_WIDTH - 1:0] out;
   
    cpu #(.ADDR_WIDTH(ADDR_WIDTH), .DATA_WIDTH(DATA_WIDTH)) CPU (.clk(slow_clk), .rst_n(rst_n), .mem(mem), .in(in), .control(control), .status(status), .we(we), .addr(addr), .data(data), .out(out), .pc(pc), .sp(sp));

    assign led[4:0] = out[4:0];
    assign led[5] = status;

    wire [3:0] ones_bcd1, tens_bcd1;
    bcd BCD1 (.in(pc[5:0]), .ones(ones_bcd1), .tens(tens_bcd1));
    ssd SSD1 (.in(ones_bcd1), .out(hex[6:0]));
    ssd SSD2 (.in(tens_bcd1), .out(hex[13:7]));
    
    wire [3:0] ones_bcd2, tens_bcd2;
    bcd BCD2 (sp[5:0], ones_bcd2, tens_bcd2);
    ssd SSD3 (.in(ones_bcd2), .out(hex[20:14]));
    ssd SSD4 (.in(tens_bcd2), .out(hex[27:21]));

    wire [15:0] ps2code;
    ps2 PS2 (.clk(clk), .rst_n(rst_n), .ps2_clk(kbd[0]), .ps2_data(kbd[1]), .code(ps2code));

    scan_codes SCAN_CODES (.clk(clk), .rst_n(rst_n), .code(ps2code), .status(status), .control(control), .num(in[3:0]));
    assign in[DATA_WIDTH - 1:4] = 0;

    wire [23:0] vgacode;
    color_codes COLOR_CODES (.num(out[5:0]), .code(vgacode));

    vga VGA (.clk(clk), .rst_n(rst_n), .code(vgacode), .hsync(mnt[13]), .vsync(mnt[12]), .red(mnt[11:8]), .green(mnt[7:4]), .blue(mnt[3:0]));

endmodule
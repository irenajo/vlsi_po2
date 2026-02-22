module vga (
    input clk,
    input rst_n,
    input [23:0] code,
    output hsync,
    output vsync,
    output [3:0] red,
    output [3:0] green,
    output [3:0] blue
);

    reg [10:0] hcount;
    reg [9:0] vcount;

    localparam H_VISIBLE = 800;
    localparam H_FRONT   = 56;
    localparam H_SYNC    = 120;
    localparam H_BACK    = 64;
    localparam H_TOTAL   = H_VISIBLE + H_FRONT + H_SYNC + H_BACK;

    localparam V_VISIBLE = 600;
    localparam V_FRONT   = 37;
    localparam V_SYNC    = 6;
    localparam V_BACK    = 23;
    localparam V_TOTAL   = V_VISIBLE + V_FRONT + V_SYNC + V_BACK;

    always @(posedge clk, negedge rst_n) begin
        if (!rst_n) begin
            hcount <= 0;
            vcount <= 0;
        end else begin
            if (hcount == H_TOTAL - 1) begin
                hcount <= 0;
                if (vcount == V_TOTAL - 1)
                    vcount <= 0;
                else
                    vcount <= vcount + 1;
            end else begin
                hcount <= hcount + 1;
            end
        end
    end

    assign hsync = ~((hcount >= (H_VISIBLE + H_FRONT)) && (hcount < (H_VISIBLE + H_FRONT + H_SYNC)));
    assign vsync = ~((vcount >= (V_VISIBLE + V_FRONT)) && (vcount < (V_VISIBLE + V_FRONT + V_SYNC)));
    assign red = hcount < H_VISIBLE && vcount < V_VISIBLE ? (hcount < H_VISIBLE / 2 ? code[23:20] : code[11:8]) : 0;
    assign green = hcount < H_VISIBLE && vcount < V_VISIBLE ? (hcount < H_VISIBLE / 2 ? code[19:16] : code[7:4]) : 0;
    assign blue = hcount < H_VISIBLE && vcount < V_VISIBLE ? (hcount < H_VISIBLE / 2 ? code[15:12] : code[3:0]) : 0;
    
endmodule
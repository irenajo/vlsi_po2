module memory #(
	parameter FILE_NAME = "mem_init.mif",
    parameter ADDR_WIDTH = 6,
    parameter DATA_WIDTH = 16
)(
    input clk,
    input we,
    input [ADDR_WIDTH - 1:0] addr,
    input [DATA_WIDTH - 1:0] data,
    output reg [DATA_WIDTH - 1:0] out
);

	(* ram_init_file = FILE_NAME *) reg [DATA_WIDTH - 1:0] mem [2**ADDR_WIDTH - 1:0];

    always @(posedge clk) begin
        if (we) begin
            mem[addr] = data;
        end
        out <= mem[addr];
    end

endmodule

/////////////
// 64 rec * 16bit , citamo u 1 taktu i pisemo u 1 taktu
// fiksna zona podataka
// adrese 00 -> 07 gpr0->gpr7

// slobodna zona podataka
// 08 -> FF 
// STEK -> od poslednje adrese u memoriji 
// PROGRAM -> od prve adrese nakon fiksne zone podataka, tj. 08
// podaci -> ??

////////////

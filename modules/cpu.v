module cpu #(
    parameter DATA_WIDTH = 16,
    parameter ADDR_WIDTH = 6,
    parameter HIGH_DATA = DATA_WIDTH-1,
    parameter HIGH_ADDR = ADDR_WIDTH-1
    
) (
    clk, rst_n, mem, in, we, addr, data, out, pc, sp

);

input clk, rst_n;
output we;

input[HIGH_DATA:0] mem, in;
output[HIGH_DATA:0] data, out;
output[HIGH_ADDR:0] addr, pc, sp;

reg [HIGH_DATA:0] out_reg, out_next;
assign out = out_reg;

// todo - za sve outpute napraviti reg

// IDEJA 1.0: STATE za svaku logicku celinu koju procesor radi
reg [6:0] state_reg, state_next; // ???


// state IR
localparam READ_IR1;
localparam READ_IR2;

// state OP
localparam INS_MOV = 4'b0000;
localparam INS_ADD = 4'b0001;

// internal CPU params


always @(posedge clk, negedge rst_n) begin
    if(!rst_n) begin
        // todo
    end
    else begin
        // setovanje vrednosti dobijenih sa OUT ?????

        // obrada STATE-a u kojem smo
        case (state_reg)
            READ_IR1: begin

            end
            READ_IR2: begin

            end

            // CPU INSTRUCTIONS
            INS_MOV: begin


            end
            INS_ADD: begin

            end
                     
            default: 
        endcase

    end
    
end







endmodule






/// STA RADI CPU

// faza:
// 1. ucitavanje i obrada instrukcije (1-2 memorijske reci)
//     - posaljemo adresu (na nju ukazuje PC) , pa PC++
//     - procitamo iz memorije, PARSIRAMO (ako je prva linija, parsiramo OC, addrA, addrB, addrC), ako je druga linija parsiramo konstantnu
//     - uvecavamo PC (za 1 ili 2, zavisi od duzine instrukcije)

// 2. obradjujemo instrukciju (MOV, ADD, IN, OUT, STOP)
//     - vidimo koji je OC i prebacujemo se u STANJE ?
//     - ucitavamo svaki parametar: pristupamo memoriji, dovlacimo u CPU (direktno i indirektno) // MOZDA PREBACITI U 1.
//     - vrsimo obradu nad podacima (add sub blablabla)
//     - stavljamo na out resenje

//     * za svaku definisemo neki kod koji je obradjuje, moze biti 1 ili vise taktova (pamtimo de smo stali pri izvrsavanju)

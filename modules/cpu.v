module cpu #(
    parameter DATA_WIDTH = 16,
    parameter ADDR_WIDTH = 6,
    parameter DATA_HIGH = DATA_WIDTH-1,
    parameter ADDR_HIGH = ADDR_WIDTH-1
    
) (
    clk, rst_n, mem, in, we, addr, data, out, pc, sp
);

input clk, rst_n;
output we;

input[DATA_HIGH:0] mem, in;
output[DATA_HIGH:0] data, out;
output[ADDR_HIGH:0] addr, pc, sp;

// instantiating variables for registers
// PC    
reg pc_cl, pc_ld, pc_inc, pc_dec, pc_sr, pc_ir, pc_sl, pc_il;
reg [5:0] pc_in;
wire [5:0] pc_out;

// SP
reg sp_cl, sp_ld, sp_inc, sp_dec, sp_sr, sp_ir, sp_sl, sp_il;
reg [5:0] sp_in;
wire [5:0] sp_out;

// IR
reg ir_cl, ir_ld, ir_inc, ir_dec, ir_sr, ir_ir, ir_sl, ir_il;
reg [31:0] ir_in;
wire [31:0] ir_out;

// MAR
reg mar_cl, mar_ld, mar_inc, mar_dec, mar_sr, mar_ir, mar_sl, mar_il;
reg [5:0] mar_in;
wire [5:0] mar_out;

// MDR
reg mdr_cl, mdr_ld, mdr_inc, mdr_dec, mdr_sr, mdr_ir, mdr_sl, mdr_il;
reg [15:0] mdr_in;
wire [15:0] mdr_out;

// ACC
reg a_cl, a_ld, a_inc, a_dec, a_sr, a_ir, a_sl, a_il;
reg [15:0] a_in;
wire [15:0] a_out;

// ALU
reg [2:0] alu_oc;
reg [DATA_WIDTH-1:0] alu_a, alu_b;
wire [DATA_WIDTH-1:0] alu_f;

// ind_x register.
reg ind_x_cl, ind_x_ld, ind_x_inc, ind_x_dec, ind_x_sr, ind_x_ir, ind_x_sl, ind_x_il;
reg [DATA_WIDTH - 1:0] ind_x_in;
wire [DATA_WIDTH - 1:0] ind_x_out;



// instantiate registers and alu
register #(6) PC(.clk(clk), .rst_n(rst_n), .cl(pc_cl), .ld(pc_ld), .in(pc_in), .inc(pc_inc), .dec(pc_dec), .sr(pc_sr), .ir(pc_ir), .sl(pc_sl), .il(pc_il), .out(pc_out));
register #(6) SP(.clk(clk), .rst_n(rst_n), .cl(sp_cl), .ld(sp_ld), .in(sp_in), .inc(sp_inc), .dec(sp_dec), .sr(sp_sr), .ir(sp_ir), .sl(sp_sl), .il(sp_il), .out(sp_out));
register #(32) IR(.clk(clk), .rst_n(rst_n), .cl(ir_cl), .ld(ir_ld), .in(ir_in), .inc(ir_inc), .dec(ir_dec), .sr(ir_sr), .ir(ir_ir), .sl(ir_sl), .il(ir_il), .out(ir_out));
register #(6) MAR(.clk(clk), .rst_n(rst_n), .cl(mar_cl), .ld(mar_ld), .in(mar_in), .inc(mar_inc), .dec(mar_dec), .sr(mar_sr), .ir(mar_ir), .sl(mar_sl), .il(mar_il), .out(mar_out));
register #(16) MDR(.clk(clk), .rst_n(rst_n), .cl(mdr_cl), .ld(mdr_ld), .in(mdr_in), .inc(mdr_inc), .dec(mdr_dec), .sr(mdr_sr), .ir(mdr_ir), .sl(mdr_sl), .il(mdr_il), .out(mdr_out));
register #(16) A(.clk(clk), .rst_n(rst_n), .cl(a_cl), .ld(a_ld), .in(a_in), .inc(a_inc), .dec(a_dec), .sr(a_sr), .ir(a_ir), .sl(a_sl), .il(a_il), .out(a_out));
    register #(.DATA_WIDTH(DATA_WIDTH)) ind_x_reg (.clk(clk),
        .rst_n(rst_n),
        .cl(ind_x_cl),
        .ld(ind_x_ld),
        .in(ind_x_in),
        .inc(ind_x_inc),
        .dec(ind_x_dec),
        .sr(ind_x_sr),
        .ir(ind_x_ir),
        .sl(ind_x_sl),
        .il(ind_x_il),
        .out(ind_x_out)
    );

alu #(16) ALU(.oc(alu_oc), .a(alu_a), .b(alu_b), .f(alu_f));


// assign output values
assign out = out_reg;
assign pc = pc_out;
assign sp = sp_out;
assign addr = mar_out;
assign data = mdr_out;

// op codes
localparam code_mov = 4'b0000;
localparam code_add = 4'b0001;
localparam code_sub = 4'b0010;
localparam code_mul = 4'b0011;
localparam code_div = 4'b0100;
localparam code_in = 4'b0111;
localparam code_out = 4'b1000;
localparam code_stop = 4'b1111;
localparam code_ir2_example = 4'b1010; // does not exist, used for testing 2-instruction length :)



// FINITE STATE MACHINE 
reg [6:0] state_reg, state_next; // ???


// FETCH, DECODE, EXECUTE, MEM_ACC??, WRITE_bACK???

// state IR
// todo : enumerate

// reset, end, error
localparam RESET = 0;
localparam ERROR; // todo

// fetch IR
localparam IR1_FETCH1_START;
localparam IR1_FETCH2_WAIT;
localparam IR1_FETCH3_loadIR;
localparam IR1_FETCH4_parse;

localparam IR2_FETCH1_START;
localparam IR2_FETCH2_WAIT;
localparam IR2_FETCH3_loadIR;
localparam IR2_FETCH4_parse;

// decode 
localparam DECODE_START; // figure out operation in this stage

localparam ADDR_X_DECODE1;   // todo - this is same logic.. we can maybe reuse these states, just that the stores are different.
localparam ADDR_X_DECODE2;
localparam ADDR_X_DECODE3;
localparam ADDR_X_DECODE4_indirect;
localparam ADDR_X_DECODE5_checkOp;


localparam IR1_SET = 6'd2;
localparam IR2_READ = 6'd3;
localparam IR2_SET = 6'd4;

// execute OP
localparam EXECUTE1;

localparam INS_MOV;
localparam INS_ADD;


// internal CPU params
reg [3:0] oc;
reg [2:0] addr1, addr2, addr3;
reg di1, di2, di3;
reg [DATA_WIDTH:0] ir2;


// wire [2:0] addr_op1_reg, addr_op1_next, addr_op2_reg, addr_op2_next, addr_op3_reg, addr_op3_next;

// assign oc = ir_out[31:28];
// assign addr1 = ir_out[26:24];
// assign addr2 = ir_out[22:20];
// assign addr3 = ir_out[18:16];
// assign i_d1 = ir_out[27];
// assign i_d2 = ir_out[23];
// assign i_d3 = ir_out[19];

/// on clock we update state and output of component
always @(posedge clk, negedge rst_n) begin
    if(!rst_n) begin
        out_reg <= {DATA_WIDTH{1'b0}};
        state_reg <= 6'd0;
    end
    else begin
        out_reg <= out_next;
        state_reg <= state_next;
    end
end

/// on any change we do actions based on the current state we are in
// todo : (does change of state on clock count too?)
always @(*) begin

    // set previous values
    out_next = out_reg;
    state_next = state_reg;
    // addr_op1_next = addr_op1_reg;
    // addr_op2_next = addr_op2_reg;
    // addr_op3_next = addr_op3_reg;

    // todo: reset values of each input of all registers (IR, MAR, MDR, etc...)

    // do current state
    case (state_reg)
        RESET: begin
            pc_ld = 1'b1;
            pc_in = 6'd8;
            sp_ld = 1'b1;
            sp_in = {6{1'b1}};
            state_next = IR1_FETCH1_START;
        end

        IR1_FETCH1_START, IR2_FETCH1_START: begin
            // put in MAR
            mar_ld = 1'b1;
            mar_in = pc;

            // change state
            case (state_reg)
                IR1_FETCH1_START : state_next =  IR1_FETCH2_WAIT;
                IR2_FETCH1_START : state_next =  IR2_FETCH2_WAIT;
                default: state_next = ERROR;
            endcase
        end

        IR1_FETCH2_WAIT, IR2_FETCH2_WAIT: begin
            // increment PC
            pc_inc = 1'b1;

            // load (from memory)
            mdr_ld = 1'b1;

            // todo what
            we_reg_next = 1'b0;
            
            // change state
            case (state_reg)
                IR1_FETCH2_WAIT : state_next =  IR1_FETCH3_loadIR;
                IR2_FETCH2_WAIT : state_next =  IR2_FETCH3_loadIR;
                default: state_next = ERROR;
            endcase
        end
        
        IR1_FETCH3_loadIR: begin
            // read from mdr into ir
            ir_ld = 1'b1; // todo remove hardwired numbers 
            ir_in[32:16] = mdr_out;
            // change state
            state_next =  IR1_FETCH4_parse;
        end

        IR2_FETCH3_loadIR: begin
            // read from mdr into ir
            ir_ld = 1'b1;
            ir_in[15:0] = mdr_out;
            // change state
            state_next =  IR2_FETCH4_parse;
        end

        IR1_FETCH4_parse: begin
            // put data in internal cpu registers
            {oc, di1, addr1, di2, addr2, di3, addr3} = ir_out[32:16];

            // based on OC, decide to either FETCH IR2, or DECODE.
            // todo
            case (oc)
                code_ir2_example: state_next = IR2_FETCH1_START;
                default: state_next = DECODE_START;
            endcase
        end

        IR2_FETCH4_parse: begin
            // put data in internal ir2 register
            ir_ld = 1'b1;
            ir_in[15:0] = mdr_out;
            ir2 = ir_out;

            // go on DECODE phase (since max length of instruction is 2 words)
            state_next = DECODE_START;
        end

        DECODE_START: begin
            case (oc)
                code_mov, 
                code_add,
                code_sub,
                code_mul: begin

                end
                code_in: begin

                end
                code_out: begin

                end
                code_div: begin
                    state_next = RESET;
                end
                code_stop: begin

                end
                default: 
            endcase
        end

        ADDR_X_DECODE1_set: begin
            // put addr of first operand (addr1) in mar
            mar_ld = 1'b1;
            mar_in = addr1;

            state_next = ADDR1_DECODE2;
        end

        ADDR1_DECODE2: begin
            // read from memory into MDR
            mdr_ld = 1'b1;

            state_next = ADDR1_DECODE3;
        end

        ADDR1_DECODE3: begin
            // read from MDR into internal register
            param1 = mdr_out;

            if(di1 == INDIRECT_READ)
                state_next = ADDR1_DECODE4_indirect_indirect;
            else
                state_next = ADDR1_DECODE5_checkOp;
        end

        ADDR1_DECODE4_indirect: begin
            // do indirect...
        end

        ADDR1_DECODE5_checkOp: begin
            case (oc)
                code_in, code_out: state_next = EXECUTE1;
                default: state_next = ADDR2_DECODE1; 
            endcase

        end

        default: 
    endcase

end



// always @(posedge clk, negedge rst_n) begin
//     if(!rst_n) begin
//         // todo
//     end
//     else begin
//         // setovanje vrednosti dobijenih sa OUT ?????

//         // obrada STATE-a u kojem smo
//         case (state_reg)
//             IR1_FETCH1_START: begin
//                 // dohvati IR1 iz memorije
//                     addr = pc;
//                     we = 1'b0;
//                     pc = pc + 1'b1;
//             end
//             IR1_SET: begin
//                     // sacekaj da prodje takt -> u mem je memorijska rec
//                     {oc, di1, addr1, di2, addr2, di3, addr3} = mem;

//                     // da li nam treba druga rec?
//                     if(OC == nesto) // todo - ovo napraviti da RADI
//                         state_reg = IR2_READ;
//                     else
//                         state_reg = OC; // ???
//                     pc = pc + 1'b1;
//             end
//             IR2_READ: begin
//                 // dohvati IR2 iz memorije
//                     addr = pc;
//                     we = 1'b0;
//                     pc = pc + 1'b1;
//             end
//             IR2_SET: begin
//                     // sacekaj da prodje takt -> u mem je memorijska rec
//                     ir2 = mem;
//                     pc = pc + 1'b1;
//             end

//             // CPU INSTRUCTIONS
//             INS_MOV: begin
                

//             end
//             INS_ADD: begin

//             end

//             default: 
//         endcase

//     end
    
// end

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

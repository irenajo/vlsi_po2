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

// internal CPU params
reg [3:0] oc;
reg [2:0] addr_x, addr_y, addr_z;
reg di_x, di_y, di_z;
reg [DATA_WIDTH:0] ir2;


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

// constants abt addressing
localparam INDIRECT_ADDRESSING = 1'b1;

// CPU op codes
localparam code_mov = 4'b0000;
localparam code_add = 4'b0001;
localparam code_sub = 4'b0010;
localparam code_mul = 4'b0011;
localparam code_div = 4'b0100;
localparam code_in = 4'b0111;
localparam code_out = 4'b1000;
localparam code_stop = 4'b1111;

// ALU op codes
localparam alu_code_add = 3'b000;
localparam alu_code_sub = 3'b001;
localparam alu_code_mul = 3'b010;
localparam alu_code_div = 3'b011;
localparam alu_code_not = 3'b100;
localparam alu_code_xor = 3'b101;
localparam alu_code_or = 3'b110;
localparam alu_code_and  = 3'b111;

// FINITE STATE MACHINE 
reg [6:0] state_reg, state_next;

// reset, end, error
localparam RESET = 0;
localparam ERROR; // todo

// fetch IR
localparam IR1_FETCH1_START;
localparam IR1_FETCH2_WAIT;
localparam IR1_FETCH_loadMDR;
localparam IR1_FETCH3_loadIR;
localparam IR1_FETCH4_parse;

localparam IR2_FETCH1_START;
localparam IR2_FETCH2_WAIT;
localparam IR2_FETCH_loadMDR;
localparam IR2_FETCH3_loadIR;
localparam IR2_FETCH4_parse;

// decode 
localparam DECODE_START; // figure out operation in this stage

localparam DECODE_X_MAR_IN;
localparam DECODE_X_MEM_READ;
localparam DECODE_X_MDR_IN;
localparam DECODE_X_INDIRECT_MAR_IN;
localparam DECODE_X_INDIRECT_MEM_READ;
localparam DECODE_X_INDIRECT_MDR_IN;
localparam DECODE_X_PUT_IN_X;

localparam DECODE_Y_MAR_IN;
localparam DECODE_Y_MEM_READ;
localparam DECODE_Y_MDR_IN;
localparam DECODE_Y_INDIRECT_MAR_IN;
localparam DECORE_Y_INDIRECT_MEM_READ;
localparam DECORE_Y_INDIRECT_MDR_IN;
localparam DECODE_Y_PUT_IN_IR;

localparam DECODE_Z_MAR_IN;
localparam DECODE_Z_MEM_READ;
localparam DECODE_Z_MDR_IN;
localparam DECODE_Z_INDIRECT_MAR_IN;
localparam DECORE_Z_INDIRECT_MEM_READ;
localparam DECORE_Z_INDIRECT_MDR_IN;
localparam DECODE_Z_PUT_IN_A;

// execute OP
localparam EXECUTE_STATE_1;
localparam EXECUTE_STATE_2;
localparam EXECUTE_STATE_3;

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

    // todo: reset values of each input of all registers (IR, MAR, MDR, etc...)
    pc_in = pc; pc_cl = 1'b0; pc_ld = 1'b0; pc_inc = 1'b0; pc_dec = 1'b0; pc_sr = 1'b0; pc_ir = 1'b0; pc_sl = 1'b0; pc_il = 1'b0;
    sp_in = sp; sp_cl = 1'b0; sp_ld = 1'b0; sp_inc = 1'b0; sp_dec = 1'b0; sp_sr = 1'b0; sp_ir = 1'b0; sp_sl = 1'b0; sp_il = 1'b0;
    ir_in = ir_out; ir_cl = 1'b0; ir_ld = 1'b0; ir_inc = 1'b0; ir_dec = 1'b0; ir_sr = 1'b0; ir_ir = 1'b0; ir_sl = 1'b0; ir_il = 1'b0;
    mar_in = mar_out; mar_cl = 1'b0; mar_ld = 1'b0; mar_inc = 1'b0; mar_dec = 1'b0; mar_sr = 1'b0; mar_ir = 1'b0; mar_sl = 1'b0; mar_il = 1'b0;
    mdr_in = mem; mdr_cl = 1'b0; mdr_ld = 1'b0; mdr_inc = 1'b0; mdr_dec = 1'b0; mdr_sr = 1'b0; mdr_ir = 1'b0; mdr_sl = 1'b0; mdr_il = 1'b0;
    a_in = a_out; a_cl = 1'b0; a_ld = 1'b0; a_inc = 1'b0; a_dec = 1'b0; a_sr = 1'b0; a_ir = 1'b0; a_sl = 1'b0; a_il = 1'b0;
    ind_x_in = ind_x_out; ind_x_cl = 1'b0; ind_x_ld = 1'b0; ind_x_inc = 1'b0; ind_x_dec = 1'b0; ind_x_sr = 1'b0; ind_x_ir = 1'b0; ind_x_sl = 1'b0; ind_x_il = 1'b0;
    alu_oc = 3'b000; alu_a = {DATA_WIDTH{1'b0}}; alu_b = {DATA_WIDTH{1'b0}}; 
    state_next = state_reg;
    out_next = out_reg;
    we_reg_next = 1'b0; // todo do we need this?

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

            // todo what
            // we_reg_next = 1'b0;
            
            // change state
            case (state_reg)
                IR1_FETCH2_WAIT : state_next =  IR1_FETCH_loadMDR;
                IR2_FETCH2_WAIT : state_next =  IR2_FETCH_loadMDR;
                default: state_next = ERROR;
            endcase
        end

        IR1_FETCH_loadMDR, IR2_FETCH_loadMDR: begin
            // load (from memory)
            mdr_ld = 1'b1;
            // (mdr_in = mem_out) by default

            // change state
            case (state_reg)
                IR1_FETCH_loadMDR : state_next =  IR1_FETCH3_loadIR;
                IR2_FETCH_loadMDR : state_next =  IR2_FETCH3_loadIR;
                default: state_next = ERROR;
            endcase
        end
        
        IR1_FETCH3_loadIR: begin
            // read from mdr into ir
            ir_ld = 1'b1; // todo remove hardwired numbers 
            ir_in[15:0] = mdr_out;
            // change state
            state_next =  IR1_FETCH4_parse;
        end

        IR2_FETCH3_loadIR: begin
            // read from mdr into ir
            ir_ld = 1'b1;
            ir_in[31:16] = mdr_out;
            // change state
            state_next =  IR2_FETCH4_parse;
        end

        IR1_FETCH4_parse: begin
            // put data in internal cpu registers
            {oc, di_x, addr_x, di_y, addr_y, di_z, addr_z} = ir_out[15:0];

            // based on OC, decide to either FETCH IR2, or DECODE.
            // however it doesnt seem that any of our instructions have 2 words length IR.
            case (oc)
                // code_ir2_example: state_next = IR2_FETCH1_START; // todo - add ops here that have 2 words in instr
                default: state_next = DECODE_START;
            endcase
        end

        IR2_FETCH4_parse: begin
            // put data in internal ir2 register
            ir_ld = 1'b1;
            ir_in[31:16] = mdr_out;
            // ir2 = ir_out; // todo ??

            // go on DECODE phase (since max length of instruction is 2 words)
            state_next = DECODE_START;
        end

        DECODE_START: begin
            // begin phase
            case (oc)
                code_mov, 
                code_add,
                code_sub,
                code_mul: begin
                        if(di_x == INDIRECT_ADDRESSING) 
                            state_next = DECODE_X_INDIRECT_MAR_IN;
                        else 
                            state_next = DECODE_Y_MAR_IN;
                    end
                code_in: begin
                        if(di_x == INDIRECT_ADDRESSING) 
                            state_next = DECODE_X_INDIRECT_MAR_IN;
                        else
                            state_next = EXECUTE_STATE_1;
                    end
                code_out: begin
                        state_next = DECODE_X_MAR_IN;
                    end
                code_div: begin
                    // ERROR
                    state_next = RESET;
                end
                code_stop: begin
                        state_next = DECODE_X_MAR_IN;
                end
            endcase;
        end

        DECODE_X_MAR_IN: begin
            // put addr of first operand (addr_x) in mar
            mar_ld = 1'b1;
            mar_in = {{{3{1'b0}}, addr_x};

            state_next = DECODE_X_MEM_READ;
        end

        DECODE_X_MEM_READ: begin
            // we_reg_next = 1'b0; // todo ?

            state_next = DECODE_X_MDR_IN;
        end

        DECODE_X_MDR_IN: begin
            mdr_ld = 1'b1;

            if(di_x == INDIRECT_ADDRESSING)
                state_next = DECODE_X_INDIRECT_MAR_IN;
            else
                state_next = DECODE_X_PUT_IN_X;
        end

        DECODE_X_INDIRECT_MAR_IN: begin
            case (oc)
                code_mov,
                code_add,
                code_sub,
                code_mul,
                code_in: begin
                    mar_in = {{3{1'b0}}, addr_x};
                end
                default: begin
                    mar_in = mdr_out[ADDR_WIDTH-1:0]; // todo, potential mismatch of lengths? how wide is mdr_out, and what will this do to it?
                end
            endcase;
            mar_ld = 1'b1;

            state_next = DECODE_X_INDIRECT_MEM_READ;
        end

        DECODE_X_INDIRECT_MEM_READ: begin
            // we = 1'b0;
            
            state_next = DECODE_X_INDIRECT_MDR_IN;
        end
        
        DECODE_X_INDIRECT_MDR_IN: begin
            // mdr_in = mem; // no need for this because i reset mdr_in = mem at beginning of combinatory block!
            mdr_ld = 1'b1;
            
            state_next = DECODE_X_PUT_IN_X;
        end

        DECODE_X_PUT_IN_X: begin
            ind_x_ld = 1'b1;
            ind_x_in = mdr_out;

            case (oc)
                MOV_OP_CODE,
                ADD_OP_CODE,
                SUB_OP_CODE,
                MUL_OP_CODE: begin
                    state_next = DECODE_Y_MAR_IN;
                end
                DIV_OP_CODE: begin
                    // ERROR
                    state_next = RESET;
                end
                STOP_OP_CODE: begin
                    state_next = DECODE_Y_MAR_IN;
                end
                IN_OP_CODE,
                OUT_OP_CODE: begin
                    state_next = EXECUTE_STATE_1;
                end
            endcase;
        end
        
        DECODE_Y_MAR_IN: begin
            // put addr of first operand (addr_y) in mar
            mar_ld = 1'b1;
            mar_in = {{3{1'b0}}, addr_y}; 

            state_next = DECODE_Y_MEM_READ;
        end

        DECODE_Y_MEM_READ: begin
            // we_reg_next = 1'b0; // todo ?

            state_next = DECODE_Y_MDR_IN;
        end

        DECODE_Y_MDR_IN: begin
            // no need: mdr_in = mem
            mdr_ld = 1'b1;

            if(di_y == INDIRECT_ADDRESSING)
                state_next = DECODE_Y_INDIRECT_MAR_IN;
            else
                state_next = DECODE_Y_PUT_IN_IR;

        end
        DECODE_Y_INDIRECT_MAR_IN: begin
            mar_in = mdr_out[ADDR_WIDTH-1:0];
            mar_ld = 1'b1;

            state_next = DECORE_Y_INDIRECT_MEM_READ;
        end

        DECORE_Y_INDIRECT_MEM_READ: begin
            // we

        state_next = DECORE_Y_INDIRECT_MDR_IN;
        end

        DECORE_Y_INDIRECT_MDR_IN: begin
            // no need for mdr_in = mem;
            mdr_ld = 1'b1;
            state_next = DECODE_Y_PUT_IN_IR;
        end

        DECODE_Y_PUT_IN_IR: begin
            // todo add register for Y when project is whole

             ir_in = {mdr_out, ir_out[15:0]}; // TODO change this!
             ir_ld = 1'b1;
                if(oc == code_in || (oc == code_stop && addr_z == 3'b000)) 
                    state_next = EXECUTE_STATE_1;
                else 
                    state_next = DECODE_Z_MAR_IN;
        end

        DECODE_Z_MAR_IN: begin
            // put addr of first operand (addr_z) in mar
            mar_ld = 1'b1;
            mar_in = {3{1'b0}, addr_z};

            state_next = DECODE_Z_MEM_READ;
        end

        DECODE_Z_MEM_READ: begin
            // we

            state_next = DECODE_Z_MDR_IN;
        end

        DECODE_Z_MDR_IN: begin
            // mdr_in = mem;
            mdr_ld = 1'b1;

            if(di_z == INDIRECT_ADDRESSING)
                state_next = DECODE_Z_INDIRECT_MAR_IN;
            else
                state_next = DECODE_Z_PUT_IN_A;
        end

        DECODE_Z_INDIRECT_MAR_IN: begin
            mar_in = mdr_out[ADDR_WIDTH-1:0];
            mar_ld = 1'b1;

            state_next = DECORE_Z_INDIRECT_MEM_READ;
        end

        DECORE_Z_INDIRECT_MEM_READ: begin
            // we
            state_next = DECORE_Z_INDIRECT_MDR_IN;
        end

        DECORE_Z_INDIRECT_MDR_IN: begin
            // mdr_in = mem;
            mdr_ld = 1'b1;

            state_next = DECODE_Z_PUT_IN_A;
        end

        DECODE_Z_PUT_IN_A: begin
            a_in = mdr_out;
            a_ld = 1'b1;
            state_next = EXECUTE_STATE_1;
        end

        EXECUTE_STATE_1: begin
            case (oc)
                code_mov: begin
                    // MOV X, Y
                    // Y => X

                    // y
                    mdr_in = ir_out[(32-1):16]; // todo, change with dedicated register reg_y!
                    mdr_ld = 1'b1;

                    // x
                    if(di_x == INDIRECT_ADDRESSING)
                        mar_in = {{3{1'b0}}, addr_x};
                    else
                        mar_in = ind_x_out[ADDR_WIDTH-1:0];
                    
                    mar_ld = 1'b1;
                    state_next = EXECUTE_STATE_2;
                end

                code_add: begin
                    // X <= Y + Z

                    // ADD
                    alu_oc = alu_code_add;
                    alu_a = ir_out[(IR_WIDTH-1):16]; // todo, change with dedicated Y register
                    alu_b = a_out; // Z

                    // in same clock, ALU will have calculated, so we can make MDR read in this clock.
                    mdr_in = alu_f;
                    mdr_ld = 1'b1;

                    if(oc == INDIRECT_ADDRESSING) 
                        mar_in = ind_x_out[(ADDR_WIDTH-1):0];
                    else 
                        mar_in = {{3{1'b0}}, addr_x};
                        
                    mar_ld = 1'b1;
                    state_next = EXECUTE_STATE_2;
                end

                code_sub: begin
                    // X <= Y - Z
                    
                    // SUB
                    alu_oc = alu_code_sub;
                    alu_a = ir_out[(IR_WIDTH-1):16]; // todo, change with dedicated Y register
                    alu_b = a_out; // Z

                    // in same clock, ALU will have calculated, so we can make MDR read in this clock.
                    mdr_in = alu_f;
                    mdr_ld = 1'b1;

                    if(oc == INDIRECT_ADDRESSING) 
                        mar_in = ind_x_out[(ADDR_WIDTH-1):0];
                    else 
                        mar_in = {{3{1'b0}}, addr_x};
                        
                    mar_ld = 1'b1;
                    state_next = EXECUTE_STATE_2;
                end

                code_mul: begin
                    // X <= Y * Z
                    
                    // SUB
                    alu_oc = alu_code_mul;
                    alu_a = ir_out[(IR_WIDTH-1):16]; // todo, change with dedicated Y register
                    alu_b = a_out; // Z

                    // in same clock, ALU will have calculated, so we can make MDR read in this clock.
                    mdr_in = alu_f;
                    mdr_ld = 1'b1;

                    if(oc == INDIRECT_ADDRESSING) 
                        mar_in = ind_x_out[(ADDR_WIDTH-1):0];
                    else 
                        mar_in = {{3{1'b0}}, addr_x};
                        
                    mar_ld = 1'b1;
                    state_next = EXECUTE_STATE_2;
                end

                code_in: begin // TODO -> how does IN (reading from stdin) WORK ???
                    // IN X

                    // status_reg_next = 1'b1; // what ????


                end

                code_out: begin
                    // OUT X
                    out_next = ind_x_out;
                    state_next = IR1_FETCH1_START; // OP DONE!
                end

                code_stop: begin
                    if(addr_x != 3'b000) begin
                        // OUT X
                        out_next = ind_x_out;
                    end 
                    state_next = EXECUTE_STATE_2;
                end
            endcase
        end

        EXECUTE_STATE_2: begin
            case (oc)
                code_mov,
                code_add,
                code_sub,
                code_mul,
                code_in: begin
                    // write into memory
                    we_reg_next = 1'b1;
                    state_next = IR1_FETCH1_START;
                end 

                code_stop: begin
                    if(addr_y != 3'b000) begin
                        // OUT Y
                        out_next = ir_out[(IR_WIDTH-1):16]; // todo, change to dedicated register
                    end 
                    reg_next = EXECUTE_STATE_3;
                end
            endcase
        end

        EXECUTE_STATE_3: begin
            if(addr_z != 3'b000) begin
                // OUT Z
                out_next = a_out;
            end 
            // spin.
            state_reg_next = IR1_FETCH1_START;
        end

        default: 
    endcase
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

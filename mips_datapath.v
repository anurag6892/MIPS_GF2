// simplified MIPS processor

// Top level module

module mips #(parameter WIDTH = 32, REGBITS = 5)
             (input              clk, reset, start,
              input  [WIDTH-1:0] instr,
              input  [WIDTH-1:0] memdata,
              output [WIDTH-1:0] instr_adr,                  // These are connected to the external instruction/data memory 
              output             memread, memwrite, irwrite, 
              output [WIDTH-1:0] data_adr, writedata,
              output             done); 

   wire        zero, alusrca, iord, pcen, regwrite, regdst; 
   wire [1:0]  aluop,pcsource,alusrcb;                // Control signals from controller module to steer the right data through muxes
   wire [2:0]  alucont, memtoreg;
   wire        tran_write, tran_read, tran_en; 
   wire        mult_write, mult_read, mult_en;
   wire        pbasis_write, pbasis_read, pbasis_en;   

   controller  cont(clk, reset, start, instr[31:26], zero, done, memread, memwrite,        
                    alusrca, iord, tran_write, tran_read, tran_en, mult_write, mult_read, mult_en, pbasis_wrtie, pbasis_read, pbasis_en, pcen, regwrite, regdst,
                    pcsource, alusrcb, aluop, memtoreg, irwrite);            // Main controller which implements an FSM and outputs control signals
   alucontrol  ac(aluop, instr[5:0], alucont);                     // Controller which interprets the signals from Main controller and generates
   datapath    #(WIDTH, REGBITS)                                  // alucont which controls the alu
               dp(clk, reset, memdata, alusrca, iord, pcen,     
                  regwrite, regdst, pcsource, alusrcb, memtoreg, alucont,
                  tran_write, tran_read, tran_en, mult_write, mult_read, mult_en, pbasis_write, pbasis_read, pbasis_en, zero, instr, data_adr, instr_adr, writedata);
    
endmodule

// Finite state machine controller
module controller(input clk, reset, start,
                  input      [5:0] op, 
                  input            zero, 
                  output           done,
                  output reg       memread, memwrite, alusrca, iord,
                  output reg       tran_write, tran_read, tran_en,
                  output reg       mult_write, mult_read, mult_en,
                  output reg       pbasis_write, pbasis_read, pbasis_en,
                  output           pcen, 
                  output reg       regwrite, regdst, 
                  output reg [1:0] pcsource, alusrcb, aluop,
                  output reg [2:0] memtoreg,
                  output reg       irwrite);
 
   /*    State Encoding        */
  
   parameter   FETCH   =  5'b00000;
   parameter   DECODE  =  5'b00001;
   parameter   MEMADR  =  5'b00010;
   parameter   LBRD    =  5'b00011;
   parameter   LBWR    =  5'b00100;
   parameter   SBWR    =  5'b00101;
   parameter   RTYPEEX =  5'b00110;
   parameter   RTYPEWR =  5'b00111;
   parameter   BEQEX   =  5'b01000;
   parameter   JEX     =  5'b01001;
   parameter   IMMEX   =  5'b01010;
   parameter   IMMWR   =  5'b01011;
   parameter   TRANL   =  5'b01100;
   parameter   TRANR   =  5'b01101;
   parameter   MULTL   =  5'b01110;
   parameter   MULTR   =  5'b01111;
   parameter   PBINVL  =  5'b10000;
   parameter   PBINVWS =  5'b10001;
   parameter   PBINVR  =  5'b10010;
   parameter   IDLE    =  5'b10011;
 
   /*  Instruction Type Encoding */
   parameter   LB      =  6'b100000;
   parameter   SB      =  6'b101000;
   parameter   RTYPE   =  6'b0;
   parameter   BEQ     =  6'b000100;
   parameter   J       =  6'b000010;
   parameter   IMM     =  6'b001000;
   parameter   TL      =  6'b010000; 
   parameter   TR      =  6'b010001;
   parameter   ML      =  6'b010010;
   parameter   MR      =  6'b010011;
   parameter   PL      =  6'b010100;
   parameter   PR      =  6'b010101;
   parameter   HLT     =  6'b111100;

   reg [4:0] state, nextstate;
   reg       pcwrite, pcwritecond;

   // state register
   always @(posedge clk)
      if(reset) state <= IDLE;
      else state <= nextstate;

   assign done = (state == IDLE) ? 1 : 0;

  /* Series of states for each type of instruction 

   LB    : FETCH DECODE MEMADR LBRD LBWR
   SB    : FETCH DECODE MEMADR SBWR
   RTYPE : FETCH DECODE RTYPEEX RTYPEWR 
   BEQ   : FETCH DECODE BEX
   J     : FETCH DECODE JEX
   IMM   : FETCH DECODE IMMEX IMMWR 
   TL    : FETCH DECODE TRNL
   TR    : FETCH DECODE TRNR
   ML    : FETCH DECODE MULTL
   MR    : FETCH DECODE MULTR
   PL    : FETCH DECODE PBINVL PBINVWS
   PR    : FETCH DECODE PBINVR
   HLT   : FETCH DECODE IDLE
  */

   // next state logic
   always @(*)
      begin
         case(state)
            IDLE:
               begin
                  if(start) nextstate <= FETCH;
                  else      nextstate  <= IDLE;
               end
      
            FETCH:  nextstate <= DECODE;
            DECODE:  case(op)
                        LB:      nextstate <= MEMADR;
                        SB:      nextstate <= MEMADR;
                        RTYPE:   nextstate <= RTYPEEX;
                        BEQ:     nextstate <= BEQEX;
                        J:       nextstate <= JEX;
                        IMM:     nextstate <= IMMEX;
                        TL:      nextstate <= TRANL;
                        TR:      nextstate <= TRANR;
                        ML:      nextstate <= MULTL;
                        MR:      nextstate <= MULTR;
                        PL:      nextstate <= PBINVL;
                        PR:      nextstate <= PBINVR;
                        HLT:     nextstate <= IDLE;
                      default: nextstate <= FETCH; // should never happen
                     endcase
            MEMADR:  case(op)
                        LB:      nextstate <= LBRD;
                        SB:      nextstate <= SBWR;
                        default: nextstate <= FETCH; // should never happen
                     endcase
            LBRD:    nextstate <= LBWR;
            LBWR:    nextstate <= FETCH;
            SBWR:    nextstate <= FETCH;
            RTYPEEX: nextstate <= RTYPEWR;
            RTYPEWR: nextstate <= FETCH;
            IMMEX:   nextstate <= IMMWR;
            IMMWR:   nextstate <= FETCH; 	
            BEQEX:   nextstate <= FETCH;
            JEX:     nextstate <= FETCH;
            TRANL:   nextstate <= FETCH;
            TRANR:   nextstate <= FETCH;
            MULTL:   nextstate <= FETCH;
            MULTR:   nextstate <= FETCH;
            PBINVL:  nextstate <= PBINVWS;
            PBINVR:  nextstate <= FETCH;
            PBINVWS: nextstate <= FETCH; 
            default: nextstate <= FETCH; // should never happen
         endcase
      end

   always @(*)
      begin
            // set all outputs to zero, then conditionally assert 
	    // just the appropriate ones
            irwrite <= 0;
            pcwrite <= 0; pcwritecond <= 0;
            regwrite <= 0; regdst <= 0;
            memread <= 0; memwrite <= 0;
            tran_write <= 0; tran_read <= 0;
            tran_en <= 0;
            mult_write <= 0; mult_read <= 0;
            mult_en <= 0 ;
            pbasis_write <= 0; pbasis_read <= 0;
            pbasis_en <= 0;
            alusrca <= 0; alusrcb <= 2'b00; aluop <= 2'b00;
            pcsource <= 2'b00;
            iord <= 0; memtoreg <= 3'b000;
            case(state)
               IDLE:
                  begin
                      
                  end
               FETCH:
                  begin
                     irwrite <= 1;
                     alusrcb <= 2'b01; 
                     pcwrite <= 1;
                  end
               DECODE: alusrcb <= 2'b11; // to calculate branch address
               MEMADR:
                  begin
                     alusrca <= 1;     
                     alusrcb <= 2'b10;   // aluout  = a + imm_se  
                  end
               LBRD:
                  begin
                     memread <= 1;      
                     iord    <= 1;     // adr = aluout
                  end
               LBWR:
                  begin
                     regwrite <= 1;
                     memtoreg <= 3'b001;    // wd = md
                  end
               SBWR:
                  begin
                     memwrite <= 1;
                     iord     <= 1;   // adr = aluout  
                  end
               RTYPEEX: 
                  begin
                     alusrca <= 1;
                     aluop   <= 2'b10;  
                  end
               RTYPEWR:
                  begin
                     regdst   <= 1;   
                     regwrite <= 1;   
                  end
               BEQEX:
                  begin
                     alusrca     <= 1;        
                     aluop       <= 2'b01;   
                     pcwritecond <= 1;
                     pcsource    <= 2'b01;
                  end
               JEX:
                  begin
                     pcwrite  <= 1;
                     pcsource <= 2'b10;
                  end
               IMMEX:
                  begin
                    alusrca <= 1;
                    alusrcb <= 2'b10; 
                  end 
               IMMWR:
                  begin
                     regwrite <= 1;
                  end
               TRANL:
                  begin
                     tran_write  <= 1;
                     tran_en     <= 1; 
                  end
               TRANR:
                  begin
                    tran_read   <= 1;
                    tran_en     <= 1;
                    memtoreg    <= 3'b010;
                    regwrite    <= 1;
                  end
               MULTL:
                  begin
                    mult_write  <= 1;
                    mult_en     <= 1;
                  end
               MULTR:
                  begin
                    mult_read   <= 1;
                    mult_en     <= 1;
                    memtoreg    <= 3'b011;
                    regwrite    <= 1;
                  end
               PBINVL:
                  begin
                    pbasis_write  <= 1;
                    pbasis_en     <= 1;
                  end
               PBINVWS:
                  begin
                    pbasis_en     <= 1;
                    memtoreg      <= 3'b111;
                    regwrite      <= 1;
                  end
               PBINVR:
                  begin
                    pbasis_read   <= 1;
                    pbasis_en     <= 1;
                    memtoreg      <= 3'b100;
                    regwrite      <= 1;
                  end
         endcase
      end
   assign pcen = pcwrite | (pcwritecond & zero); // program counter enable
endmodule

// Generate control bits for ALU
module alucontrol(input      [1:0] aluop, 
                  input      [5:0] funct, 
                  output reg [2:0] alucont);

   always @(*)
      case(aluop)
         2'b00: alucont <= 3'b010;  // add for lb/sb
         2'b01: alucont <= 3'b110;  // sub (for beq)
         default: case(funct)       // R-Type instructions
                     6'b100000: alucont <= 3'b010; // add (for add)
                     6'b100010: alucont <= 3'b110; // subtract (for sub)
                     6'b100100: alucont <= 3'b000; // logical and (for and)
                     6'b100101: alucont <= 3'b001; // logical or (for or)
                     6'b101010: alucont <= 3'b111; // set on less (for slt)
                     default:   alucont <= 3'b101; // should never happen
                  endcase
      endcase
endmodule

// Datapath, including register file, ALU, muxes, and other registers
module datapath #(parameter WIDTH = 32, REGBITS = 5)
                 (input              clk, reset, 
                  input  [WIDTH-1:0] memdata, 
                  input              alusrca, iord, 
		  input              pcen, regwrite, regdst,
                  input  [1:0]       pcsource, alusrcb, 
                  input  [2:0]       memtoreg, alucont,
                  input              tran_write, tran_read, tran_en,
                  input              mult_write, mult_read, mult_en,
                  input              pbasis_write, pbasis_read, pbasis_en, 
                  output             zero, 
                  input  [WIDTH-1:0] instr, 
                  output [WIDTH-1:0] data_adr, instr_adr, writedata);

   // the size of the parameters must be changed to match the WIDTH parameter
   localparam CONST_ZERO = 32'b0;
   localparam CONST_ONE =  32'b1;
   
   wire [REGBITS-1:0] ra1, ra2, wa;
   wire [WIDTH-1:0]   pc, nextpc, md, rd1, rd2, wd, a, src1, src2, aluresult,
                          aluout, imm_se, jump_adr, branch_adr;
   wire [WIDTH-1:0] load_top, transpose_colwise;
   wire [WIDTH-1:0] mult_col_a, mult_row_b, mult_result;
   wire [WIDTH-1:0] pbasis_cr, pbasis_out;   
   wire [WIDTH-1:0] top_net, bot_net;
   wire status;

   assign instr_adr = pc;
  
   assign load_top = a;  
  
   assign mult_col_a = a;
   assign mult_row_b = writedata;
      
   assign  pbasis_cr = a;
     
   // shift left constant field by 2
   assign jump_adr = { pc[31:26], instr[25:0] };
   assign imm_se   = { 16'hffff*instr[15], instr[15:0] };
   assign branch_adr = { 16'b11111111111111*instr[15],instr[15:0] };  
 
   // register file address fields
   assign ra1 = instr[REGBITS+20:21];
   assign ra2 = instr[REGBITS+15:16];
   mux2       #(REGBITS) regmux(instr[REGBITS+15:16], 
                                instr[REGBITS+10:11], regdst, wa);

   
   // datapath
   flopenr    #(WIDTH)  pcreg(clk, reset, pcen, nextpc, pc);
   flop       #(WIDTH)  mdr(clk, memdata, md);
   flop       #(WIDTH)  areg(clk, rd1, a);	
   flop       #(WIDTH)  wrd(clk, rd2, writedata);
   flop       #(WIDTH)  res(clk, aluresult, aluout);
   mux2       #(WIDTH)  adrmux(pc, aluout, iord, data_adr);
   mux2       #(WIDTH)  src1mux(pc, a, alusrca, src1);
   mux4       #(WIDTH)  src2mux(writedata, CONST_ONE, imm_se, 
                                branch_adr, alusrcb, src2);
   mux4       #(WIDTH)  pcmux(aluresult, aluout, jump_adr, CONST_ZERO, 
                                pcsource, nextpc);
   // mux8 using 2 mux4s and 1 mux2
   mux4       #(WIDTH)  wdmux1(aluout, md, transpose_colwise, mult_result, memtoreg[1:0],top_net);
   mux4       #(WIDTH)  wdmux2(pbasis_out, CONST_ZERO, CONST_ZERO, CONST_ONE, { memtoreg[1], memtoreg[0] && status }, bot_net);
   mux2       #(WIDTH)  wdmux3(top_net, bot_net, memtoreg[2], wd);
   
   regfile    #(WIDTH, REGBITS) rf(clk, regwrite, ra1, ra2, wa, wd, rd1, rd2);
   alu        #(WIDTH) alunit(src1, src2, alucont, aluresult);
   zerodetect #(WIDTH) zd(aluresult, zero);
   gfm_transpose #(WIDTH) transpose(clk, tran_en, reset, tran_write, tran_read, load_top, transpose_colwise);
   gfm_mac_pack #(WIDTH) mult(clk, mult_en, reset, mult_write, mult_read, mult_col_a, mult_row_b, mult_result);
   pbasis_builder #(WIDTH) pb(clk, reset, pbasis_en,  pbasis_write, pbasis_read, pbasis_cr, pbasis_out, status);

endmodule

module alu #(parameter WIDTH = 32)
            (input      [WIDTH-1:0] a, b, 
             input      [2:0]       alucont, 
             output reg [WIDTH-1:0] result);

   wire     [WIDTH-1:0] b2, sum, slt;

   assign b2 = alucont[2] ? ~b:b; 
   assign sum = a + b2 + alucont[2];
   // slt should be 1 if most significant bit of sum is 1
   assign slt = sum[WIDTH-1];

   always@(*)
      case(alucont[1:0])
         2'b00: result <= a & b;
         2'b01: result <= a | b;
         2'b10: result <= sum;
         2'b11: result <= slt;
      endcase
endmodule

module regfile #(parameter WIDTH = 32, REGBITS = 5)
                (input                clk, 
                 input                regwrite, 
                 input  [REGBITS-1:0] ra1, ra2, wa, 
                 input  [WIDTH-1:0]   wd, 
                 output [WIDTH-1:0]   rd1, rd2);

   reg  [WIDTH-1:0] RAM [(1<<REGBITS)-1:0];

   // three ported register file
   // read two ports combinationally
   // write third port on rising edge of clock
   // register 0 hardwired to 0
   
  
   always @(posedge clk) begin
      if (regwrite) RAM[wa] <= wd;	
      RAM[0] <= 32'b0;
   end
 
   assign rd1 = ra1 ? RAM[ra1] : 0;
   assign rd2 = ra2 ? RAM[ra2] : 0;
endmodule

module zerodetect #(parameter WIDTH = 32)
                   (input [WIDTH-1:0] a, 
                    output            y);

   assign y = (a==0);
endmodule	

module flop #(parameter WIDTH = 32)
             (input                  clk, 
              input      [WIDTH-1:0] d, 
              output reg [WIDTH-1:0] q);

   always @(posedge clk)
      q <= d;
endmodule

module flopen #(parameter WIDTH = 32)
               (input                  clk, en,
                input      [WIDTH-1:0] d, 
                output reg [WIDTH-1:0] q);

   always @(posedge clk)
      if (en) q <= d;
endmodule

module flopenr #(parameter WIDTH = 32)
                (input                  clk, reset, en,
                 input      [WIDTH-1:0] d, 
                 output reg [WIDTH-1:0] q);
 
   always @(posedge clk)
      if      (reset) q <= 0;
      else if (en)    q <= d;
endmodule

module mux2 #(parameter WIDTH = 32)
             (input  [WIDTH-1:0] d0, d1, 
              input              s, 
              output [WIDTH-1:0] y);

   assign y = s ? d1 : d0; 
endmodule

module mux4 #(parameter WIDTH = 32)
             (input      [WIDTH-1:0] d0, d1, d2, d3,
              input      [1:0]       s, 
              output reg [WIDTH-1:0] y);

   always @(*)
      case(s)
         2'b00: y <= d0;
         2'b01: y <= d1;
         2'b10: y <= d2;
         2'b11: y <= d3;
      endcase
endmodule

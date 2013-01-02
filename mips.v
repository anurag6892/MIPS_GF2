//-------------------------------------------------------
// mips_mem.v - connect mips to memory
// Anurag Reddy
//-------------------------------------------------------

// top level design includes both mips processor and memory
module mips_mem #(parameter WIDTH = 32, REGBITS = 5)
   ( input clk, reset, start, write_instr, write_data, read_data,
     input [WIDTH-1:0] instr_in, instr_write_adr,
     input [WIDTH-1:0] data_in, data_adr,
     output [WIDTH-1:0] data_out,
     output done );
   
   wire    memread, irwrite;
   wire    memwrite_mips;
   wire    [WIDTH-1:0] data_adr_mips, data_address, instr_read_adr, instr_out, writedata, writedata_mips;
   wire    [WIDTH-1:0] memdata;
   wire    en = 1; 
   wire    instr_mem_en;
   wire    datawrite;   
   

   assign instr_mem_en = irwrite || write_instr;
   
   assign datawrite = memwrite_mips || write_data;

   // instantiate the mips processor
   mips #(WIDTH,REGBITS) mips(.clk(clk), .reset(reset), .start(start), .memdata(data_out), .instr_adr(instr_read_adr), .instr(instr_out), .memread(memread), .memwrite(memwrite_mips), .irwrite(irwrite), .data_adr(data_adr_mips), .writedata(writedata_mips), .done(done));
   
   // instantiate memory for code and data
   instr_mem #(WIDTH) instr_mem(.clk(clk), .en(instr_mem_en), .reset(reset), .write_valid(write_instr), .read_adr(instr_read_adr), .write_adr(instr_write_adr), .instr_in(instr_in), .instr_out(instr_out));
  
   data_mem #(WIDTH) data_mem(.clk(clk), .en(en), .reset(reset), .write_valid(datawrite), .adr(data_address), .writedata(writedata), .memdata(data_out)); 
   
   // mux2   #(WIDTH) instradrmux(instr_read_adr, instr_write_adr, write_instr, instr_adr);
   
   mux2   #(WIDTH) dataadrmux(data_adr_mips, data_adr, done, data_address);
 
   mux2   #(WIDTH) datainmux(writedata_mips, data_in, done, writedata);
 
endmodule

/*module mux2 #(parameter WIDTH = 32)
             (input  [WIDTH-1:0] d0, d1,
              input              s,
              output [WIDTH-1:0] y);

   assign y = s ? d1 : d0;
endmodule
*/

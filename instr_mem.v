//-------------------------------------------------------
// Model of 32-bit memory for the mips processor. 
// Anurag Reddy
//-------------------------------------------------------
module instr_mem #(parameter WIDTH = 32, INSTR_MEM_ADDR_BITS = 32 )
   (input clk, en, reset, write_valid, 
    input [INSTR_MEM_ADDR_BITS-1:0] read_adr,
    input [INSTR_MEM_ADDR_BITS-1:0] write_adr,
    input [WIDTH-1:0] instr_in,
    output reg [WIDTH-1:0] instr_out
    );

   reg [WIDTH-1:0] instr_mem [31:0];
  
   integer i=0; 
 
   always @(negedge clk)
     begin
        if (reset) begin
              for (i=0; i<32; i=i+1)
                    begin
                       instr_mem[i] <= 32'b0;
                    end
            end  
     end

 // The behavioral description of the RAM - note clocked behavior
   always @(negedge clk)
      if (en) begin
        instr_out <= instr_mem[read_adr];
        if(write_valid)
             begin
                   instr_mem[write_adr] <= instr_in; 
             end 
      end
endmodule

//-------------------------------------------------------
// Model of 32-bit memory for the mips processor. 
// Anurag Reddy
//-------------------------------------------------------
module data_mem #(parameter WIDTH = 32, DATA_MEM_ADDR_BITS = 32 )
   (input clk, en, reset, write_valid, 
    input [DATA_MEM_ADDR_BITS-1:0] adr,
    input [WIDTH-1:0] writedata,
    output reg [WIDTH-1:0] memdata
    );

   reg [WIDTH-1:0] data_mem [127:0];
    

 // The behavioral description of the RAM - note clocked behavior
   always @(negedge clk)
      if (en) begin
         if (write_valid)
            data_mem[adr] <= writedata;    
         memdata <= data_mem[adr];
      end

endmodule

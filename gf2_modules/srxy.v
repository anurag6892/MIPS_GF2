// Description:
// N-Registers 
// 1. that can load in our out along y-direction
// 2. or, circular shift left or right (along, x-direction)
// 3. or, stay-put
module srxy #(parameter N=32)
(
    input wire          clk,
    input wire          reset,
    input wire  [1:0]   sel,
    input wire  [N-1:0] load_in,
    output wire [N-1:0] load_out
);
localparam [1:0] 
nop = 2'b00,
    shl = 2'b01,
    shr = 2'b10,
    shd = 2'b11;
//
//
reg [N-1:0] sr, sr_next;
//
//
assign load_out = sr;
//
//
always@(posedge clk, posedge reset)
begin
    if(reset)
        sr <= 0;
    else
        sr <= sr_next;
end
//
//
always@(*)
begin
    case(sel)
        nop:sr_next <= sr;
        shl:sr_next <= {sr[N-2:0], sr[N-1]};    
        shr:sr_next <= {sr[0], sr[N-1:1]};
        shd:sr_next <= load_in;                    
    endcase
end

endmodule

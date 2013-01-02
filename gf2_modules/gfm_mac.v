// Description:
//
module gfm_mac #(parameter N=32)
(
    input wire          clk,
    input wire          clk_en,
    input wire          reset,
    input wire  [1:0]   sel,
    input wire  [N-1:0] load_top,
    input wire  [N-1:0] load_side,
    output wire [N-1:0] load_down
);

function [N-1:0] pick_nextstate
    (  
       input reg [N-1:0] vtop,
       input reg [N-1:0] vside, 
       input reg [N-1:0] vthis,
       input reg [1:0]   sel
    );
    reg [N-1:0] result;
    begin
        case(sel)
            2'b00:result = vthis;
            2'b01:result = vthis;
            2'b10:result = vthis ^ vside;
            2'b11:result = vtop;
        endcase
        pick_nextstate = result;
    end
endfunction

reg [N-1:0] sr, sr_next;

always@(posedge clk, posedge reset)
begin
    if(reset)
        sr <= 0;
    else if(clk_en)
        sr <= pick_nextstate(load_top, load_side, sr, sel);
end

assign load_down = sr;

endmodule

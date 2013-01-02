//
//
//
module gfm_transpose #(parameter N=32)
(
    clk,
    clk_en,
    reset,
    write_valid,
    read_valid,
    load_top,
    transpose_colwise
);
input           clk;
input           clk_en;
input           reset;
input  [N-1:0]  load_top;
input           write_valid;
input           read_valid;
output [N-1:0]  transpose_colwise;
//
//
wire       [N-1:0]  ldnets [0:N-1+1];
wire       [N-1:0]  shnet;
wire       [1:0]    sel_internal;
//
//
assign  transpose_colwise = shnet;
assign  ldnets[0]         = load_top;
assign  sel_internal = pick_sel(read_valid, write_valid, clk_en);
//
//
function [1:0] pick_sel
    (
        input reg read_valid, 
        input reg write_valid,
        input reg clk_en
    );
    begin
        pick_sel = 0;
        if(clk_en) begin
            if(read_valid)
                pick_sel = 2'b01;
            else if(write_valid)
                pick_sel = 2'b11;
        end
    end
endfunction
//
//
genvar gn;
generate
for(gn=0; gn<N; gn=gn+1)
begin:SRXY 
srxy srxy(clk,
    reset, 
    sel_internal,
    ldnets[gn], 
    ldnets[gn+1]
);
assign shnet[gn] = ldnets[gn+1][N-1];
end
endgenerate

endmodule

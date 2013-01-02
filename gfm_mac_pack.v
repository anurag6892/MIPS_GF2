// Description:
// Computes FF Matrix Multiplication by repeated accumulation of
// outerproducts.
//
// The outer product of the current pair of ith row and ith column
// is computed in a combinational fashion, then, the results are
// wired-up to a x-y connected register network , where this fresh
// outer product is xor-accumulated onto the sum of outer products so far.
//

module gfm_mac_pack #(parameter N = 32)
(
    input             clk,
    input             clk_en,
    input             reset,
    input             write_valid,
    input             read_valid,
    input  [N-1:0]    col_a,
    input  [N-1:0]    row_b,
    output [N-1:0]    load_down
);

wire   [1:0]     sel_xy;
wire   [N-1:0]   side_load  [0:N-1];
wire   [N-1:0]   ldnets     [0:N-1+1];

assign ldnets[0] = 0;
assign load_down = ldnets[N];
assign sel_xy    = pick_sel(read_valid, write_valid);

function [1:0] pick_sel
    (
        input reg read_valid, 
        input reg write_valid
    );
    begin
		  pick_sel = 0;
        if(clk_en)
        begin
            if(read_valid)
                pick_sel = 2'b11;
            else if(write_valid)
                pick_sel = 2'b10;
        end
    end
endfunction

genvar gn;
generate
for(gn=0; gn<N; gn=gn+1)
begin:FMGF2
    gfm_mac gfmac
    (
        .clk(clk),
        .clk_en(clk_en),
        .reset(reset), 
        .sel(sel_xy), 
        .load_top(ldnets[gn]),
        .load_side(side_load[gn]),
        .load_down(ldnets[gn+1])
    );
end
endgenerate

generate
for(gn=0; gn<N; gn=gn+1)
begin:LNKS
    assign side_load[gn] = row_b * col_a[gn];          
end
endgenerate

endmodule

module lnz #(
    parameter N = 32
)
(
    input          clk,
    input          reset,
    input   [N-1:0] v,
    output  [N-1:0] vlnz
);

function [15:0] u16_lnz
    (
        input reg [15:0] u16v
    );
    reg [15:0] out;
    begin
        out = 0;
        if(u16v[15])
            out[15] = 1;
        else if(u16v[14])
            out[14] = 1;
        else if(u16v[13])
            out[13] = 1;
        else if(u16v[12])
            out[12] = 1;
        else if(u16v[11])
            out[11] = 1;
        else if(u16v[10])
            out[10] = 1;
        else if(u16v[9])
            out[9] = 1;
        else if(u16v[8])
            out[8] = 1;
        else if(u16v[7])
            out[7] = 1;
        else if(u16v[6])
            out[6] = 1;
        else if(u16v[5])
            out[5] = 1;
        else if(u16v[4])
            out[4] = 1;
        else if(u16v[3])
            out[3] = 1;
        else if(u16v[2])
            out[2] = 1;
        else if(u16v[1])
            out[1] = 1;
        else if(u16v[0])
            out[0] = 1;

        u16_lnz = out;
    end
endfunction
function or16
    (
        input reg [15:0] v
    );
    integer i;
    reg out;
    begin
        out = 0;
        for(i=0; i<16; i=i+1)
            out = out + v[i];

        or16 = out;
    end
endfunction

wire [15:0] xh, xl; 
wire        XhHas1;

assign xh = u16_lnz(v[N-1:16]);
assign xl = u16_lnz(v[15:0]);
assign XhHas1 = or16(xh);
assign vlnz = (XhHas1)?({xh, 16'h0000}):({16'h0000, xl});

endmodule

module pbasis_builder #(parameter  N=32)
(
    input           clk,
    input           reset,
    input           clk_en,
    input           write_valid,
    input           read_valid,
    input  [31:0]   cr,
    output [31:0]   out,
    output          status 
);

reg  [4:0]   count_pbasis;
reg  [4:0]   count_pbasis_next;
reg  [5:0]   count_readout;


reg  [31:0]  pbasis               [0:31];
reg  [31:0]  pbasis_next          [0:31];
reg  [32*32-1:0]  
             pbasis_tobe_xored;


reg  [31:0]  pbasis_inverse       [0:31];
reg  [31:0]  pbasis_inverse_next  [0:31];
reg  [32*32-1:0]  
             pbasis_inverse_tobe_xored;  

reg          cr_accepted;

reg  [31:0] reduced_cr;

wire [31:0] pbasis_lnz           [0:31];
wire [31:0] reduced_cr_lnz;
wire [31:0] cr_lnz;

integer i, j;
//---------------------------------------------------------------------------
assign status           = cr_accepted; 
assign cr_lnz           = u32_lnz(cr);
assign reduced_cr_lnz   = u32_lnz(reduced_cr);

//---------------------------------------------------------------------------
genvar gn;
generate
for(gn=0; gn<32; gn=gn+1)
begin:CLNZ
    assign pbasis_lnz[gn] = u32_lnz(pbasis[gn]);
end
endgenerate
//---------------------------------------------------------------------------
function integer log2;
    input integer value;
    begin
        value = value-1;
        for (log2=0; value>0; log2=log2+1)
            value = value>>1;
    end
endfunction
function [3:0] u4_lnz
    (
        input reg [3:0] u4v
    );
    reg [3:0] out;
    begin
        out = 0;
        if(u4v[3])
            out[3] = 1;
        else if(u4v[2])
            out[2] = 1;
        else if(u4v[1])
            out[1] = 1;
        else if(u4v[0])
            out[0] = 1;
        
        u4_lnz = out;
    end
endfunction

function [7:0] u8_lnz
    (
        input reg [7:0] u8v
    );
    reg [7:0] out;
    reg [3:0] xh;
    reg [3:0] xl;
    reg XhHas1;
    begin
        out = 0;
        xh = u4_lnz(u8v[7:4]);
        xl = u4_lnz(u8v[3:0]);
        XhHas1 = or4(xh);
        out = (XhHas1)?({xh, 4'h0}):({4'h0, xl});
        u8_lnz = out;
    end
endfunction
function [15:0] u16_lnz
    (
        input reg [15:0] u16v
    );
    reg [15:0] out;
    reg [7:0] xh;
    reg [7:0] xl;
    reg XhHas1;
    begin
        out = 0;
        xh = u8_lnz(u16v[15:8]);
        xl = u8_lnz(u16v[7:0]);
        XhHas1 = or8(xh);
        out = (XhHas1)?({xh, 8'h00}):({8'h00, xl});
        u16_lnz = out;
    end
endfunction
function or4
    (
        input reg [3:0] v
    );
    integer i;
    reg out;
    begin
        out = 0;
        for(i=0; i<4; i=i+1)
            out = out + v[i];

        or4 = out;
    end
endfunction
function or8
    (
        input reg [7:0] v
    );
    integer i;
    reg out;
    begin
        out = 0;
        for(i=0; i<8; i=i+1)
            out = out + v[i];

        or8 = out;
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
// While being reduced by the current pbasis rows,
// the LNZ of the candidate row needs to be updated repeatedly in procedural
// loop, where a continuous assignment won't work (or the simulator panics)
// SO: This makeshift solution
function [31:0] u32_lnz
    (
        input reg [31:0] u32v
    );
    reg [31:0] out;
    reg [15:0] xh;
    reg [15:0] xl;
    reg XhHas1;
    begin

        xh = u16_lnz(u32v[31:16]);
        xl = u16_lnz(u32v[15:0]);
        XhHas1 = or16(xh);
        out = (XhHas1)?({xh, 16'h0000}):({16'h0000, xl});

        u32_lnz = out;
    end
endfunction

function [31:0] XOR_Tree
    (
        input reg [31:0] A,
        input reg [32*32-1:0] T
    );
    integer i, j;
    begin
    XOR_Tree=A;
    for(i=0; i<32; i=i+1) 
      for(j=0; j<32; j=j+1) 
        XOR_Tree[j] = XOR_Tree[j] ^ T[i*32+j];
    end
endfunction


//-----------------------------------------------------------------------------
// 1 HUGE Combinational Block
//-----------------------------------------------------------------------------
always@(clk, reset, clk_en, cr, count_pbasis, pbasis[i], pbasis_inverse[i])
begin
    cr_accepted = 0;
    reduced_cr = cr;
    count_pbasis_next = count_pbasis;

    for(i=0; i<32; i=i+1) 
    begin
        pbasis_next[i] = pbasis[i];
        pbasis_inverse_next[i] = pbasis_inverse[i];
    end

    // (cr & u32_lnz(pbasis[i])) is nonzero => 
    // the reduced_cr can be reduced further =>
    // we do it.
    //
    // ALSO, in case, the reduced_cr turns out to be a valid basis,
    // we compute the possible next state candidate for the corresponding row
    // of the Inverse

    //for(i=0; i<count_pbasis; i=i+1), actually, but this is better
    for(i=0; i<32; i=i+1) 
    begin
        if(pbasis_lnz[i] & cr) 
        begin
          //      pbasis_tobe_xored[i*32:i*32+31] = pbasis[i*32:i*32+31];
          //      pbasis_inverse_tobe_xored[i*32:i*32+31] = 
          //          pbasis_inverse[i*32:i*32+31];
            for(j=0; j<32; j=j+1) begin
                pbasis_tobe_xored[i*32+j] = pbasis[i][j];
                pbasis_inverse_tobe_xored[i*32+j] = pbasis_inverse[i][j];
            end
        end 
        else 
        begin
           //     pbasis_tobe_xored[i*32:i*32+31] = 0;
           //     pbasis_inverse_tobe_xored[i*32:i*32+31] = 0;
            for(j=0; j<32; j=j+1) begin
                pbasis_tobe_xored[i*32+j] = 0;
                pbasis_inverse_tobe_xored[i*32+j] = 0;
            end
        end
    end

    reduced_cr = XOR_Tree(cr, pbasis_tobe_xored);

    // Raise the flag
    if(reduced_cr)
        cr_accepted = 1;


    if(cr_accepted)
    begin
        pbasis_inverse_next[count_pbasis] = 
            XOR_Tree(pbasis_inverse[count_pbasis],
            pbasis_inverse_tobe_xored);        
    end

    // Use this final reduced_cr to zero-out all the 1's
    // in pbases in the u32_lnz(reduced_cr) column. 
    // Pick only those pbases that have 1's in this position
    //
    // Also, apply them on Inverse too.

    if(cr_accepted)
    begin
    //for(i=0; i<count_pbasis; i=i+1), actually, but this is better
    for(i=0; i<32; i=i+1) 
        begin
            if(reduced_cr_lnz & pbasis[i])
            begin
              pbasis_next[i] = pbasis[i] ^ reduced_cr;
              pbasis_inverse_next[i] =  pbasis_inverse[i]^
                  pbasis_inverse_next[count_pbasis];
            end
        end
    end

    // We are Here:
    // At this point, either the cr is rejected, OR it is accepted with
    // cr being reduced to its 'minimal form', and relevant pbasis too are
    // ridden off the 1's in unwanted places

    if(cr_accepted)
    begin
        pbasis_next[count_pbasis] = reduced_cr;
        count_pbasis_next = count_pbasis + 1'b1;
    end


end


always@(posedge clk, posedge reset)
begin
    if(reset)
    begin
        count_pbasis <= 0;
        for(i=0; i<32; i=i+1)
        begin
            pbasis[i] <= 0;
            pbasis_inverse[i] <= 1<<(31-i);
        end
    end
    else
    begin
        count_pbasis <= count_pbasis_next;
        for(i=0; i<32; i=i+1)
        begin
            pbasis[i] <= pbasis_next[i];
            pbasis_inverse[i] <= pbasis_inverse_next[i];
        end

        //------------
        // DEBUG
        //------------
       /* if(count_pbasis_next==0)
            for(i=0; i<32; i=i+1)
                $display("%d %32b", pbasis_inverse_next[i],
            pbasis_inverse_next[i]);  */
        //------------

    end
end

//-------------------------------
// READBACK
assign   out = (count_readout<32)?
    pbasis[count_readout]:
    pbasis_inverse[count_readout-32];
always@(posedge clk or posedge reset)
begin
    if(reset)
        count_readout <= 0;
    else if((count_pbasis==0) && read_valid && (!write_valid))
        count_readout <= count_readout + 1;
		  
end
endmodule

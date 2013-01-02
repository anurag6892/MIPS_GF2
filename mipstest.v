//-------------------------------------------------------
// Test the mips together with memory
// Anurag Reddy
//-------------------------------------------------------

// top level design for testing 

module top #(parameter WIDTH = 32, REGBITS = 5)();
 
   reg  clk = 0;
   reg  reset = 1;
   reg  start = 0;
   reg  write_instr = 0;
   reg  write_data = 0 ;
   reg  read_data = 0 ; 
   reg  [WIDTH-1:0] instr_in = 32'b0; 
   reg  [WIDTH-1:0] instr_write_adr = 32'b0 ;
   reg  [WIDTH-1:0] data_in = 32'b0; 
   reg  [WIDTH-1:0] data_adr = 32'b0;
   wire [WIDTH-1:0] data_out;
   wire done;
 
   reg [WIDTH-1:0] instr_mem [27:0];
    
   
   initial 
      begin
                 instr_mem[0] <= 32'h20010000;    // addi $1, $0, 0
                 instr_mem[1] <= 32'h20020020;    // addi $2, $0, 32
                 instr_mem[2] <= 32'h10220004;    // loop1: beq $1, $2, label1
                 instr_mem[3] <= 32'h80230000;    // lw $1, $3, 0 
                 instr_mem[4] <= 32'h40600000;    // tl $3, $0, 0
                 instr_mem[5] <= 32'h20210001;    // addi $1, $1, 1
                 instr_mem[6] <= 32'h08000002;    // j loop1
                 instr_mem[7] <= 32'h20420020;    // label1: addi $2, $2, 32
                 instr_mem[8] <= 32'h10220004;    // loop2: beq $1, $2, label2
                 instr_mem[9] <= 32'h44030000;    // tr $0, $3
                 instr_mem[10] <= 32'hA0230000;   // sw $1, $3, 0
                 instr_mem[11] <= 32'h20210001;   // addi $1, $1, 1
                 instr_mem[12] <= 32'h08000008;   // j loop2
                 instr_mem[13] <= 32'h20010000;   // label 2: addi $1, $0, 0
                 instr_mem[14] <= 32'h20020020;   // addi $2, $0, 32
                 instr_mem[15] <= 32'h10220005;   // loop3: beq $1, $2, label3
                 instr_mem[16] <= 32'h80240020;   // lw $1, $4, 32
                 instr_mem[17] <= 32'h80250040;   // lw $1, $5, 64
                 instr_mem[18] <= 32'h48850000;   // ml $4, $5, 0
                 instr_mem[19] <= 32'h20210001;   // addi $1, $1, 1
                 instr_mem[20] <= 32'h0800000f;   // j loop3
                 instr_mem[21] <= 32'h20420020;   // label3: addi $2, $2, 32 
                 instr_mem[22] <= 32'h10220004;   // loop4: beq $1, $2, end
                 instr_mem[23] <= 32'h4c060000;   // mr $0, $6, 0
                 instr_mem[24] <= 32'hA0260040;   // sw $1, $6, 64
                 instr_mem[25] <= 32'h20210001;   // addi $1, $1, 1
                 instr_mem[26] <= 32'h08000016;   // j loop4 
                 instr_mem[27] <= 32'hf0000000;   // end: halt
   end
  
   integer i = 0;

   // instantiate devices to be tested
   mips_mem #(WIDTH,REGBITS) dut(clk, reset, start, write_instr, write_data, read_data, instr_in, instr_write_adr, data_in, data_adr, data_out, done); 

   // initialize the test, then quit after a while
   initial
      begin
         reset <= 1; 
         #20 reset <= 0;
           
         for(i=0; i<28; i=i+1)
            begin
                instr_in <= instr_mem[i];
            
                write_instr <= 1;
                
                #10 begin 
                           write_instr <= 0;
                           instr_write_adr <= instr_write_adr + 1;
                    end                
                #10  begin  end           
            end
            
         #5 // to synchronize with negedge
 
         for (i=0; i<32; i=i+1)
                    begin
                        data_in <=  32'h80000000 >> (i);
                        write_data <= 1 ;  
                        #10

                        write_data  <= 0;
                        data_adr <= data_adr + 1;

                        #10 begin  end                 
                    end

         for (i=32; i<64; i=i+1)
                    begin
                        data_in <= 32'b0;
                        write_data <= 1;
          
                        #10

                        write_data  <= 0;
                        data_adr <= data_adr + 1;
                          
                        #10 begin  end
                   end
           

         for (i=64; i<96; i=i+1)
                    begin
                        data_in <= 32'h80000000 >> (i-64);     
                        write_data <= 1;  
                      
                        #10

                        write_data <= 0;
                        data_adr <= data_adr + 1;

                        #10 begin end
                    end
 
        for (i=96; i<128; i= i+1)
                    begin
                        data_in <= 32'b0;
                        write_data <= 1;                    
                       
                        #10

                        write_data <= 0;
                        data_adr <= data_adr + 1;

                        #10 begin end
                    end
        
         #10
         start <= 1;
      
         #20
         start <= 0;
                
         #25000 $finish;
        
      end
  
   // generate clock to sequence the tests
   always #5 clk <= ~clk; 

   
  
   // check the data on the memory interface of the mips_dut
   // Check whenever the memwrite signal is active
   always@(negedge clk)
      begin
         if(dut.datawrite)
            if(dut.data_address == 32'h0000003f & dut.writedata == 32'h00000001)
               begin
                 $display("Transpose Successful!!!");    
               end
         if(dut.datawrite)
            if(dut.data_address == 32'h0000007f & dut.writedata == 32'h00000001)
               begin
                 $display("Multiplication Successfull!!!");
               end 
      end
     
endmodule


module main_tb();
    reg clk;

    reg __failed;
    
    initial begin
        $dumpfile(`VCD_OUTPUT);
        $dumpvars(0,  main_tb);
        clk = 0;
        __failed = 0;
        forever begin
            #1 clk = ~clk;
        end
    end

    initial begin
        #100000
        `END_TEST;
    end

    top uut(.CLK(clk));
endmodule

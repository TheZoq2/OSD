// look in pins.pcf for all the pin names on the TinyFPGA BX board
module top (
    input CLK,    // 16MHz clock
    output LED,   // User/boot LED next to power LED
    output USBPU,  // USB pull-up resistor

    // Debug pin
    output PIN_13,

    output PIN_5,
    output PIN_6,
    output PIN_7,
    output PIN_8,
    output PIN_9,
    output PIN_10,
    output PIN_11,
    output PIN_12
);
    reg rst = 1;
    always @(posedge CLK) begin
        rst <= 0;
    end

    reg[15:0] t = 'hffff;

    always @(posedge CLK) begin
        t <= t + 1;
    end

    // assign PIN_13 = 1;


    wire[4:0] videoSignal;

    frameTracker ft
        ( .clk(CLK)
        , .rst(rst)
        , .analogValue(videoSignal)
        , .newFrame(PIN_13)
        );




    assign PIN_12 = 1'b0;
    assign PIN_11 = 1'b0;
    assign PIN_10 = videoSignal[0];
    assign PIN_9 = videoSignal[1];
    assign PIN_8 = videoSignal[2];
    assign PIN_7 = videoSignal[3];
    assign PIN_6 = videoSignal[4];
    assign PIN_5 = 1'b0;
endmodule



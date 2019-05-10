// look in pins.pcf for all the pin names on the TinyFPGA BX board
module top (
    input CLK,    // 16MHz clock
    output LED,   // User/boot LED next to power LED
    output USBPU,  // USB pull-up resistor

    // Debug pin
    output PIN_13,

    output PIN_23,
    output PIN_22,
    output PIN_21,
    output PIN_20,
    output PIN_19
);
    reg[15:0] t = 'hffff;

    always @(posedge CLK) begin
        t <= t + 1;
    end

    assign PIN_13 = CLK;
    // assign PIN_13 = 1;


    wire[4:0] videoSignal;

    frameTracker ft(.clk(CLK), .rst(0), .analogValue(videoSignal));




    assign PIN_23 = videoSignal[0];
    assign PIN_22 = videoSignal[1];
    assign PIN_21 = videoSignal[2];
    assign PIN_20 = videoSignal[3];
    assign PIN_19 = videoSignal[4];
endmodule



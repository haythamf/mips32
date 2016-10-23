// Code your testbench here
// or browse Examples
module testbench();

  reg        clk;
  reg        reset;

  wire [31:0] writedata, dataadr;
  wire        memwrite;

  // instantiate device to be tested
  top dut(clk, reset, writedata, dataadr, memwrite);

  // initialize test
  initial
    begin

      integer i;
      reset <= 1; # 22; reset <= 0;
      for(i=0;i<5;i=i+1) begin
        @(negedge clk);
        $display(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\ncycle=%d\ninstr=%h\npc=%h\nPCBranch=%h",
        i,
        dut.instr,
        dut.pc,
        dut.mips.dp.pcbranch,
        );
        $display("===============control signals===================");
      	check_c;
        $display("===============rf values===================");
        check_rf;
        $display("===============dmem values================");
        check_dmem;
       	$display("End of Cycle%d\n<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<", i);
      end
      $stop();

    end

  // generate clock to sequence tests
  always
    begin
      clk <= 1; # 5; clk <= 0; # 5;
    end

  //tasks
  task check_c;
    begin
      $display("branch=%b\nALUControl=%b\nALUSrc=%b\nsrca=%h\nsrcb=%h\naluout=%h\nzero=%h\npcsrc=%h",
      dut.mips.c.branch,
      dut.mips.c.alucontrol,
      dut.mips.c.alusrc,
      dut.mips.dp.srca,
      dut.mips.dp.srcb,
      dut.mips.dp.aluout,
      dut.mips.dp.zero,
      dut.mips.c.pcsrc);
    end
  endtask

  task check_rf;
    begin
      $display("wa3=%d(d) contains wd3=%h",
      dut.mips.dp.rf.wa3,
      dut.mips.dp.rf.wd3);

    end
  endtask

  task check_dmem;
    begin
      $display("memwrite=%b\ndataadr=%h\nwritedata=%h\nreaddata=%h",
      memwrite,
      dataadr,
      writedata,
      dut.readdata);
    end
  endtask

endmodule

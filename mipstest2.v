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
      $dumpfile("dump.vcd"); $dumpvars;
      reset <= 1;
      #12;
      reset <= 0;
      for(i=0;i<1;i=i+1) begin
        @(negedge clk);
        $display(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\ncycle=%d\ninstr=%h\npc=%h\nPCBranch=%h\npcnext=%h",
        i,
        dut.instr,
        dut.pc,
        dut.mips.dp.pcbranch,
        dut.mips.dp.pcnext);
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
      $display("branch=%b\nALUControl=%b\nALUSrc=%b\nsrca=%h\nsrcb=%h\naluout=%h\nzero=%h\npcsrc=%h\njump=%b\njal=%b",
      dut.mips.c.branch,
      dut.mips.c.alucontrol,
      dut.mips.c.alusrc,
      dut.mips.dp.srca,
      dut.mips.dp.srcb,
      dut.mips.dp.aluout,
      dut.mips.dp.zero,
      dut.mips.c.pcsrc,
      dut.mips.c.jump,
      dut.mips.c.jal);
    end
  endtask

  task check_rf;
    begin
      $display("wa3=%d(d) contains wd3=%h\nregwrite=%b\nra1=%d(d)\trd1=%h\nra2=%d(d)\trd2=%h",
      dut.mips.dp.rf.wa3,
      dut.mips.dp.rf.wd3,
      dut.mips.dp.rf.we3,
      dut.mips.dp.rf.ra1,
      dut.mips.dp.rf.rd1,
      dut.mips.dp.rf.ra2,
      dut.mips.dp.rf.rd2);

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

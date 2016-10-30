module top(input         clk, reset,
           output [31:0] writedata, dataadr,
           output        memwrite);

  wire [31:0] pc, instr, readdata;

  // instantiate processor and memories
  mips mips(clk, reset, pc, instr, memwrite, dataadr, writedata, readdata);
  imem imem(pc[7:2], instr);
  dmem dmem(clk, memwrite, dataadr, writedata, readdata);

endmodule

module mips(input          clk, reset,
            output  [31:0] pc,
            input   [31:0] instr,
            output         memwrite,
            output  [31:0] aluout, writedata,
            input   [31:0] readdata);

  wire         memtoreg,
               pcsrc, zero,
               regdst, regwrite, jump, jal;
  wire [1:0] alusrc;
  wire [2:0]  alucontrol;

  controller c(instr[31:26], instr[5:0], zero,
               memtoreg, memwrite, pcsrc,
               alusrc, regdst, regwrite, jump,
               jal, alucontrol);
  datapath dp(clk, reset, memtoreg, pcsrc,
              alusrc, regdst, regwrite, jump, jal,
              alucontrol,
              zero, pc, instr,
              aluout, writedata, readdata);
endmodule

module controller(input   [5:0] op, funct,
                  input         zero,
                  output        memtoreg, memwrite,
                  output        pcsrc,
                  output  [1:0] alusrc,
                  output        regdst, regwrite,
                  output        jump, jal,
                  output  [2:0] alucontrol);

  wire [1:0] aluop;
  wire       branch;
  wire       bne;

  maindec md(op, memtoreg, memwrite, branch, bne,
             alusrc, regdst, regwrite, jump, jal,
             aluop);
  aludec  ad(funct, aluop, alucontrol);

  assign pcsrc = branch & (bne ^ zero);
endmodule

module maindec(input   [5:0] op,
               output        memtoreg, memwrite,
               output        branch,
               //bne is added
               output        bne,
               //alusrc is modified
               output  [1:0] alusrc,
               output        regdst, regwrite,
               output        jump, jal,
               output  [1:0] aluop);

  reg [11:0] controls;

  assign {regwrite, regdst, alusrc,
          branch, memwrite,
          memtoreg, jump, aluop, bne, jal} = controls; // expanded

  always_comb
    case(op)
      6'b000000: controls <= 12'b110000001000; //Rtype
      6'b100011: controls <= 12'b100100100000; //LW
      6'b101011: controls <= 12'b000101000000; //SW
      6'b000100: controls <= 12'b000010000100; //BEQ
      6'b001000: controls <= 12'b100100000000; //ADDI
      6'b000010: controls <= 12'b000000010000; //J
      6'b000011: controls <= 12'b100000010001; //jal
      6'b001101: controls <= 12'b101000001100; // ORI, ALUOp=11 (OR)
      6'b000101: controls <= 12'b000010000110; // BNE
      default:   controls <= 12'bxxxxxxxxxxxx; //???
    endcase
endmodule

module aludec(input   [5:0] funct,
              input   [1:0] aluop,
              output reg [2:0] alucontrol);

  always@(*)
    case(aluop)
      2'b00: alucontrol <= 3'b010;  // add
      2'b01: alucontrol <= 3'b110;  // sub
      2'b11: alucontrol <= 3'b001;  // or
      default: case(funct)          // RTYPE
          6'b100000: alucontrol <= 3'b010; // ADD
          6'b100010: alucontrol <= 3'b110; // SUB
          6'b100100: alucontrol <= 3'b000; // AND
          6'b100101: alucontrol <= 3'b001; // OR
          6'b101010: alucontrol <= 3'b111; // SLT
          default:   alucontrol <= 3'bxxx; // ???
        endcase
    endcase
endmodule

module datapath(input          clk, reset,
                input          memtoreg, pcsrc,
                // expanded for ori
                input   [1:0]  alusrc,
                input          regdst,
                input          regwrite, jump, jal,
                input   [2:0]  alucontrol,
                output         zero,
                output  [31:0] pc,
                input   [31:0] instr,
                output  [31:0] aluout, writedata,
                input   [31:0] readdata);

  wire [4:0]  wraddr1_r, wraddr2_r;
  wire [31:0] pcnext, pcnextbr, pcplus4, pcplus8, pcbranch;
  wire [31:0] signimm, zeroimm, signimmsh;
  wire [31:0] srca, srcb;
  wire [31:0] result, writedata_r;

  // next PC logic
  flopr #(32) pcreg(clk, reset, pcnext, pc);
  adder       pcadd1(pc, 32'b100, pcplus4);
  sl2         immsh(signimm, signimmsh);
  /*module adder(input   [31:0] a, b,
               output  [31:0] y);*/
  adder       pcadd2(pcplus4, signimmsh, pcbranch);
  //pcadd3 to produce pcplus8
  //pcplus8 used in (Jal)
  adder       pcadd3(pcplus4, 32'b100, pcplus8);
  mux2 #(32)  pcbrmux(pcplus4, pcbranch, pcsrc,
                      pcnextbr);
  mux2 #(32)  pcmux(pcnextbr, {pcplus4[31:28],
                    instr[25:0], 2'b00},
                    jump, pcnext);

  // register file logic
  regfile     rf(clk, regwrite, instr[25:21],
                 instr[20:16], wraddr2_r,
                 writedata_r, srca, writedata);
  //this mux to chose the Return reg R[31] in case of Jal
  mux2 #(5)   returnaddr_mux(wraddr1_r, 5'b1111_1, jal, wraddr2_r);
  mux2 #(5)   wrmux(instr[20:16], instr[15:11],
                    regdst, wraddr1_r);
  mux2 #(32)  pclinkmux(result, pcplus8,
                     jal, writedata_r);
  mux2 #(32)  resmux(aluout, readdata,
                     memtoreg, result);
  signext     se(instr[15:0], signimm);

  zeroext     ze(instr[15:0], zeroimm);

  // ALU logic
  // srcbmux is modified to handle ori
  mux4 #(32)  srcbmux(writedata, signimm, zeroimm, ,alusrc,
                      srcb);

  alu32         alu(.A(srca), .B(srcb), .F(alucontrol),
                  .Y(aluout), .Zero(zero));

  //
endmodule

module regfile(input          clk,
               input          we3,
               input   [4:0]  ra1, ra2, wa3,
               input   [31:0] wd3,
               output  [31:0] rd1, rd2);

  reg [31:0] rf[31:0];

  // three ported register file
  // read two ports combinationally
  // write third port on rising edge of clock
  // register 0 hardwired to 0

  always @(posedge clk)
    if (we3) rf[wa3] <= wd3;

  assign rd1 = (ra1 != 0) ? rf[ra1] : 0;
  assign rd2 = (ra2 != 0) ? rf[ra2] : 0;
endmodule

module adder(input   [31:0] a, b,
             output  [31:0] y);

  assign y = a + b;
endmodule

module sl2(input   [31:0] a,
           output  [31:0] y);

  // shift left by 2
  assign y = {a[29:0], 2'b00};
endmodule

module signext(input   [15:0] a,
               output  [31:0] y);

  assign y = {{16{a[15]}}, a};
endmodule

module zeroext(input   [15:0] a,
               output  [31:0] y);

  assign y = {{16{1'b0}}, a};
endmodule

module flopr #(parameter WIDTH = 8)
              (input               clk, reset,
               input      [WIDTH-1:0] d,
               output reg [WIDTH-1:0] q);

  always @(posedge clk, posedge reset)
    if (reset) q <= 0;
    else       q <= d;
endmodule

module flopenr #(parameter WIDTH = 8)
                (input               clk, reset,
                 input               en,
                 input   [WIDTH-1:0] d,
                 output reg [WIDTH-1:0] q);

  always @(posedge clk, posedge reset)
    if      (reset) q <= 0;
    else if (en)    q <= d;
endmodule

module mux2 #(parameter WIDTH = 8)
             (input   [WIDTH-1:0] d0, d1,
              input               s,
              output  [WIDTH-1:0] y);

  assign y = s ? d1 : d0;
endmodule

module mux4 #(parameter WIDTH = 32)
             (input   [WIDTH-1:0] d0, d1, d2, d3,
              input   [1:0]       s,
              output  [WIDTH-1:0] y);

  assign y = s[1] ? (s[0] ? d3: d2) : (s[0] ? d1: d0);
endmodule

module alu32( input [31:0] A, B, input [2:0] F,
              output reg [31:0] Y,
              output Zero);
    wire [31:0] S, Bout;

    assign Bout = F[2] ? ~B : B;
    assign S = A + Bout + F[2];

    always @ (*)
      case (F[1:0])
        2'b00: Y <= A & Bout;
        2'b01: Y <= A | Bout;
        2'b10: Y <= S;
        2'b11: Y <= S[31];
      endcase

      assign Zero = (Y == 32'b0);

endmodule

module dmem(input          clk, we,
            input   [31:0] a, wd,
            output  [31:0] rd);

  reg [31:0] RAM[63:0];

  assign rd = RAM[a[31:2]]; // word aligned

  always @(posedge clk)
    if (we)
      RAM[a[31:2]] <= wd;
endmodule

module imem(input   [5:0]   a,
            output  [31:0]   rd);

  reg [31:0] ROM[63:0];

  initial
    begin
      $readmemh("memfile3.dat",ROM); // initialize memory
    end

  assign rd = ROM[a]; // word aligned
endmodule

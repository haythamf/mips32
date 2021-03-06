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
               regdst, regwrite, jump, jal, jr,
               lui;
  wire [1:0] alusrc;
  wire [3:0]  alucontrol;

  controller c(instr[31:26], instr[5:0], zero,
               memtoreg, memwrite, pcsrc,
               alusrc, regdst, regwrite, jump,
               jal, lui, alucontrol);
  datapath dp(clk, reset, memtoreg, pcsrc,
              alusrc, regdst, regwrite, jump, jal,
              lui, alucontrol, zero, pc, instr,
              aluout, writedata, readdata);
endmodule

module controller(input   [5:0] op, funct,
                  input         zero, ltez,
                  output        memtoreg, memwrite,
                  output        pcsrc,
                  output  [1:0] alusrc,
                  output        regdst, regwrite,
                  output        jump, jal, lui,
                  output  [3:0] alucontrol);

  wire [2:0] aluop;
  wire       branch;
  wire       bne;
  wire       blez;


  maindec md(op, memtoreg, memwrite, branch, bne,
             alusrc, regdst, regwrite, jump, jal, lui,
             aluop);
  aludec  ad(funct, aluop, alucontrol);

  assign pcsrc = (branch & (bne ^ zero)) | (ltez & blez);
endmodule

module maindec(input   [5:0] op,
               output        memtoreg, memwrite,
               output        branch,
               //bne is added
               output        bne,
               //alusrc is modified
               output  [1:0] alusrc,
               output        regdst, regwrite,
               output        jump, jal, lui,
               output  [2:0] aluop);

  reg [13:0] controls;

  assign {regwrite, regdst, alusrc,
          branch, memwrite,
          memtoreg, jump, aluop, bne, jal, lui} = controls; // expanded

  always_comb
    case(op)
      6'b000000: controls <= 14'b11000000111000; //Rtype
      6'b100011: controls <= 14'b10010010000000; //LW
      6'b101011: controls <= 14'b00010100000000; //SW
      6'b000100: controls <= 14'b00001000001000; //BEQ
      6'b001000: controls <= 14'b10010000000000; //ADDI
      6'b000010: controls <= 14'b00000001000000; //J
      6'b000011: controls <= 14'b10000001000010; //jal
      6'b001101: controls <= 14'b10100000010000; // ori, ALUOp=011 (OR)
      6'b001100: controls <= 14'b10100000011000; // andi, ALUOp=000 (AND)
      6'b001110: controls <= 14'b10100000100000; // xori, ALUOp=000 (AND)
      6'b000101: controls <= 14'b00001000001100; // BNE
      6'b001111: controls <= 14'b10000000000001; //lui
      6'b001010: controls <= 14'b10010000101000; //slti
      default:   controls <= 14'bxxxxxxxxxxxxxx; //???
    endcase
endmodule

module aludec(input   [5:0] funct,
              input   [2:0] aluop,
              output reg [3:0] alucontrol);

  always@(*)
    case(aluop)
      3'b000: alucontrol <= 4'b0010;  // add
      3'b001: alucontrol <= 4'b1010;  // sub
      3'b010: alucontrol <= 4'b0001;  // or
      3'b011: alucontrol <= 4'b0000;  // and
      3'b100: alucontrol <= 4'b0111;  // xor
      3'b101: alucontrol <= 4'b1011;  // slt
      default: case(funct)          // RTYPE
          6'b100000: alucontrol <= 4'b0010; // ADD
          6'b100010: alucontrol <= 4'b1010; // SUB
          6'b100100: alucontrol <= 4'b0000; // AND
          6'b100101: alucontrol <= 4'b0001; // OR
          6'b101010: alucontrol <= 4'b1011; // SLT
          6'b000000: alucontrol <= 4'b0100; // sll
          6'b000010: alucontrol <= 4'b0101; // srl
          6'b000011: alucontrol <= 4'b0110; // sra
          default:   alucontrol <= 4'bxxx; // ???
        endcase
    endcase
endmodule

module datapath(input          clk, reset,
                input          memtoreg, pcsrc,
                // expanded for ori
                input   [1:0]  alusrc,
                input          regdst,
                input          regwrite, jump, jal, lui,
                input   [3:0]  alucontrol,
                output         zero,
                output  [31:0] pc,
                input   [31:0] instr,
                output  [31:0] aluout, writedata,
                input   [31:0] readdata);

  wire [4:0]  wraddr1_r, wraddr2_r;
  wire [31:0] pcnext, pcnextbr, pcplus4, pcplus8, pcbranch;
  wire [31:0] signimm, zeroimm, signimmsh;
  wire [31:0] srca, srcb;
  wire [31:0] result, wd3;

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
                 wd3, srca, writedata);
  //this mux to chose the Return reg R[31] in case of Jal
  mux2 #(5)   returnaddr_mux(wraddr1_r, 5'b1111_1, jal, wraddr2_r);
  mux2 #(5)   wrmux(instr[20:16], instr[15:11],
                    regdst, wraddr1_r);
  mux4 #(32)  wd3mux(result, pcplus8, {instr[15:0],16'b0}, 32'bz,
                     {lui,jal}, wd3);
  mux2 #(32)  resmux(aluout, readdata,
                     memtoreg, result);
  signext     se(instr[15:0], signimm);

  zeroext     ze(instr[15:0], zeroimm);

  // ALU logic
  // srcbmux is modified to handle ori
  mux4 #(32)  srcbmux(writedata, signimm, zeroimm, ,alusrc,
                      srcb);

  alu32         alu(srca, srcb, alucontrol, instr[10:6],
                  aluout, zero);

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

module alu32( input [31:0] A, B, input [3:0] F,
              input [4:0] shamt,
              output reg [31:0] Y,
              output Zero,
              output ltez//less than or equal to zero);

    wire [31:0] S, Bout;

    assign Bout = F[3] ? ~B : B;
    assign S = A + Bout + F[3];

    always @ (*)
      case (F[2:0])
        3'b000: Y <= A & Bout;
        3'b001: Y <= A | Bout;
        3'b010: Y <= S;
        3'b011: Y <= S[31];
        3'b100: Y <= Bout << shamt;
        3'b101: Y <= Bout >> shamt;
        3'b110: Y <= Bout >>> shamt;
        default: Y <= A ^ Bout;
      endcase

      assign Zero = (Y == 32'b0);
      assign ltez = (Y == 32'b0) | S[31];

endmodule

module sll (input [31:0]  a,
            input [4:0]   shamt,
            output [31:0] y);

  assign y=a << shamt;

endmodule

module srl (input [31:0]  a,
            input [4:0]   shamt,
            output [31:0] y);

  assign y=a >> shamt;

endmodule

module sra (input [31:0]  a,
            input [4:0]   shamt,
            output [31:0] y);

  assign y=a >>> shamt;

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

/* Generated by Yosys 0.7 (git sha1 UNKNOWN, clang 8.0.0 -fPIC -Os) */

module top(clk, rst, g_input, e_input, o);
  wire _00_;
  wire _01_;
  wire _02_;
  wire _03_;
  wire _04_;
  wire _05_;
  wire _06_;
  wire _07_;
  wire _08_;
  wire _09_;
  wire _10_;
  wire _11_;
  wire _12_;
  wire _13_;
  wire _14_;
  wire _15_;
  wire _16_;
  wire _17_;
  wire _18_;
  wire _19_;
  wire _20_;
  wire _21_;
  wire _22_;
  wire _23_;
  wire _24_;
  wire _25_;
  wire _26_;
  wire _27_;
  wire _28_;
  wire _29_;
  wire _30_;
  wire _31_;
  wire _32_;
  wire _33_;
  input clk;
  input [2:0] e_input;
  input [2:0] g_input;
  output [1:0] o;
  input rst;
  IV _34_ (
    .A(g_input[0]),
    .Z(_33_)
  );
  IV _35_ (
    .A(g_input[1]),
    .Z(_00_)
  );
  AND _36_ (
    .A(g_input[2]),
    .B(_00_),
    .Z(_01_)
  );
  NAND _37_ (
    .A(_01_),
    .B(_33_),
    .Z(_02_)
  );
  NAND _38_ (
    .A(_02_),
    .B(g_input[2]),
    .Z(_03_)
  );
  XOR _39_ (
    .A(e_input[2]),
    .B(g_input[2]),
    .Z(_04_)
  );
  XOR _40_ (
    .A(e_input[0]),
    .B(g_input[0]),
    .Z(_05_)
  );
  XOR _41_ (
    .A(e_input[1]),
    .B(g_input[1]),
    .Z(_06_)
  );
  OR _42_ (
    .A(_06_),
    .B(_05_),
    .Z(_07_)
  );
  OR _43_ (
    .A(_07_),
    .B(_04_),
    .Z(_08_)
  );
  OR _44_ (
    .A(e_input[1]),
    .B(e_input[0]),
    .Z(_09_)
  );
  NAND _45_ (
    .A(_09_),
    .B(e_input[2]),
    .Z(_10_)
  );
  AND _46_ (
    .A(_10_),
    .B(_08_),
    .Z(_11_)
  );
  AND _47_ (
    .A(_11_),
    .B(_03_),
    .Z(o[0])
  );
  NAND _48_ (
    .A(_10_),
    .B(_03_),
    .Z(_12_)
  );
  AND _49_ (
    .A(e_input[1]),
    .B(_00_),
    .Z(_13_)
  );
  OR _50_ (
    .A(_13_),
    .B(e_input[0]),
    .Z(_14_)
  );
  OR _51_ (
    .A(e_input[1]),
    .B(_00_),
    .Z(_15_)
  );
  AND _52_ (
    .A(_15_),
    .B(_05_),
    .Z(_16_)
  );
  AND _53_ (
    .A(_16_),
    .B(_14_),
    .Z(_17_)
  );
  NOR _54_ (
    .A(_13_),
    .B(_05_),
    .Z(_18_)
  );
  OR _55_ (
    .A(e_input[2]),
    .B(g_input[2]),
    .Z(_19_)
  );
  OR _56_ (
    .A(_19_),
    .B(_18_),
    .Z(_20_)
  );
  OR _57_ (
    .A(_20_),
    .B(_17_),
    .Z(_21_)
  );
  IV _58_ (
    .A(e_input[0]),
    .Z(_22_)
  );
  OR _59_ (
    .A(e_input[2]),
    .B(_22_),
    .Z(_23_)
  );
  OR _60_ (
    .A(_23_),
    .B(_02_),
    .Z(_24_)
  );
  IV _61_ (
    .A(e_input[2]),
    .Z(_25_)
  );
  OR _62_ (
    .A(e_input[1]),
    .B(_25_),
    .Z(_26_)
  );
  OR _63_ (
    .A(e_input[0]),
    .B(g_input[0]),
    .Z(_27_)
  );
  OR _64_ (
    .A(_27_),
    .B(g_input[2]),
    .Z(_28_)
  );
  OR _65_ (
    .A(_28_),
    .B(_26_),
    .Z(_29_)
  );
  AND _66_ (
    .A(_29_),
    .B(_24_),
    .Z(_30_)
  );
  AND _67_ (
    .A(_30_),
    .B(_08_),
    .Z(_31_)
  );
  AND _68_ (
    .A(_31_),
    .B(_21_),
    .Z(_32_)
  );
  OR _69_ (
    .A(_32_),
    .B(_12_),
    .Z(o[1])
  );
endmodule

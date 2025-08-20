package z80

import chisel3._
import chisel3.util._
//import common.Consts._

class ALU16 extends Module {
  val io = IO(new Bundle {
    val input_register = Input(UInt(16.W))
    val offset = Input(SInt(8.W))
    val output = Output(UInt(16.W))
  })

  // Add signed 8-bit offset to 16-bit base with wrap-around, preserving unsigned base semantics
  val sum = (Cat(0.U(1.W), io.input_register).asSInt + io.offset)
  val sumU = sum.asUInt
  io.output := sumU(15,0)
/*
  println("hoge")
  println(io.output)
  */
}


class ALU extends Module {
  val io = IO(new Bundle {
    val input_A = Input(UInt(8.W))
    val input_B = Input(UInt(8.W))
    val input_carry = Input(UInt(1.W))
    val calc_type = Input(UInt(8.W))
    val output_C = Output(UInt(8.W))
    val flag = Output(UInt(8.W))
  })

  val parity2_tbl = VecInit(Seq(1.U,0.U,0.U,1.U))
  def getParity(temp:UInt):UInt =
    parity2_tbl(Cat(
      parity2_tbl(Cat(parity2_tbl(temp(7,6)), parity2_tbl(temp(5,4)))),
      parity2_tbl(Cat(parity2_tbl(temp(3,2)), parity2_tbl(temp(1,0))))
    ))

  val add_op = 0x80.U
  val adc_op = 0x88.U
  val add_adc_op = 0x08.U
  val sub_op = 0x90.U
  val sbc_op = 0x98.U
  val sub_sbc_op = 0x09.U
  val and_op = 0xA0.U
  val xor_op = 0xA8.U
  val and_xor_op = 0x0A.U
  val or_op = 0xB0.U
  val cp_op = 0xB8.U
  val or_cp_op = 0x0B.U

  io.output_C := 0.U

  io.flag := RegInit(0.U(8.W))

  var S = WireDefault(1.B)
  var Z = WireDefault(1.B)
  var H = WireDefault(1.B)
  var PV = WireDefault(1.B)
  var X = WireDefault(1.B)
  var N = WireDefault(1.B)
  var C = WireDefault(1.B)

  val temp = WireDefault(0.U(16.W))
  val temps = WireDefault(0.S(16.W))

  val temph = WireDefault(0.U(8.W))

  val parity = WireDefault(0.U(8.W))


  switch(io.calc_type(7,4)) {
    is(add_adc_op)  {
        temp := Cat(0.U(1.W), io.input_A) + Cat(0.U(1.W), io.input_B) + (io.calc_type(3) & io.input_carry)
        temph := 0.U(16.W) + io.input_A(3,0) + io.input_B(3,0) + (io.calc_type(3) & io.input_carry)
        H := temph(4)
        PV := ~(io.input_A(7) ^ io.input_B(7)) & (io.input_A(7) ^ temp(7))
        N := 0.B
        C := temp(8)
        io.output_C := temp
    }
    is(sub_sbc_op) {
      temp := Cat(0.U(1.W), io.input_A) - Cat(0.U(1.W), io.input_B) - (io.calc_type(3) & io.input_carry)
      temph := 0.U(16.W) + io.input_A(3,0) - io.input_B(3,0) - (io.calc_type(3) & io.input_carry)
      H := temph(4)
      PV := (io.input_A(7) ^ io.input_B(7)) & (io.input_A(7) ^ temp(7))
      N := 1.B
      C := temp(8)
      io.output_C := temp
    }
    is(and_xor_op) {
      temp := Mux(io.calc_type(3), io.input_A ^ io.input_B , io.input_A & io.input_B)
      H := 1.U
      PV := getParity(temp)
      N := 0.U
      C := 0.U
      io.output_C := temp
    }
    is(or_cp_op) {
      switch(io.calc_type) {
        is(or_op) {
          temp := io.input_A | io.input_B
          H := 1.U
          PV := getParity(temp)
          N := 0.U
          C := 0.U
          io.output_C := temp
        }
        is(cp_op) {
          temp := Cat(0.U(1.W), io.input_A) - Cat(0.U(1.W), io.input_B)
          temph := 0.U(16.W) + io.input_A(3,0) - io.input_B(3,0)
          H := temph(4)
          PV :=(io.input_A(7) | io.input_B(7))^temp(7)
          N := 1.B
          C := temp(8)
          io.output_C := io.input_A  // no change
        }
      }
    }
  }

  S := temp(7)
  Z := (temp(7,0) === 0.U(8.W))

  io.flag := Cat(S, Z, X, H, X, PV, N, C)
}


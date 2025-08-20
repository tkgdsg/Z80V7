// See README.md for license details.

package z80

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

/**
  * This is a trivial example of how to run this Specification
  * From within sbt use:
  * {{{
  * testOnly gcd.GCDSpec
  * }}}
  * From a terminal shell use:
  * {{{
  * sbt 'testOnly gcd.GCDSpec'
  * }}}
  * Testing from mill:
  * {{{
  * mill %NAME%.test.testOnly gcd.GCDSpec
  * }}}
  */
class ALUSpec extends AnyFreeSpec with Matchers with ChiselSim {

  "ALU should perform addition" in {
    simulate(new ALU()) { dut =>

      def printlnFlag(flag: UInt): Unit = {
        val v = flag.peek().litValue               // BigInt
        val bin8 = ("00000000" + v.toString(2)).takeRight(8)
        println(s"flag=0b$bin8")
      }
//      val test = 2
//      val testValues = for { x <- 0 to 10; y <- 0 to 10} yield (x, y)
//      val inputSeq = testValues.map { case (x, y) => (new GcdInputBundle(16)).Lit(_.value1 -> x.U, _.value2 -> y.U) }
//      val resultSeq = testValues.map { case (x, y) =>
//        (new GcdOutputBundle(16)).Lit(_.value1 -> x.U, _.value2 -> y.U, _.gcd -> BigInt(x).gcd(BigInt(y)).U)

      // simple addition
      dut.io.input_A.poke(5)
      dut.io.input_B.poke(10)
      dut.io.calc_type.poke(0x80.U) // ADD operation
      dut.clock.step()
      dut.io.output_C.expect(15)
      dut.io.flag.expect(0x28.U) // No flags set for addition

      // half carry test
      dut.io.input_A.poke(15)
      dut.io.input_B.poke(1)
      dut.io.calc_type.poke(0x80.U) // ADD operation
      dut.clock.step()
      dut.io.output_C.expect(16)
//      Console.out.println(f"flag=0x${dut.io.flag.peek().litValue}%02X")
      dut.io.flag.expect(0x38.U) // No flags set for addition

      // half carry test
      dut.io.input_A.poke(15)
      dut.io.input_B.poke(1)
      dut.io.calc_type.poke(0x80.U) // ADD operation
      dut.clock.step()
      dut.io.output_C.expect(16)
//      Console.out.println(f"flag=0x${dut.io.flag.peek().litValue}%02X")
      dut.io.flag.expect(0x38.U) // No flags set for addition

      // zero flag
      dut.io.input_A.poke(0)
      dut.io.input_B.poke(0)
      dut.io.calc_type.poke(0x80.U) // ADD operation
      dut.clock.step()
      dut.io.output_C.expect(0)
//      Console.out.println(f"flag=0x${dut.io.flag.peek().litValue}%02X")
      dut.io.flag.expect(0x68.U) // No flags set for addition

      // full carry but not half carry
      dut.io.input_A.poke(0x80.U)
      dut.io.input_B.poke(0x80.U)
      dut.io.calc_type.poke(0x80.U) // ADD operation
      dut.clock.step()
      dut.io.output_C.expect(0)
      Console.out.println(f"flag=0x${dut.io.flag.peek().litValue}%02X")
      dut.io.flag.expect(0x6D.U) // No flags set for addition

      // full carry
      dut.io.input_A.poke(0xFF.U)
      dut.io.input_B.poke(0x01.U)
      dut.io.calc_type.poke(0x80.U) // ADD operation
      dut.clock.step()
      dut.io.output_C.expect(0)
      Console.out.println(f"flag=0x${dut.io.flag.peek().litValue}%02X")
      dut.io.flag.expect(0x79.U) // No flags set for addition
     }
  }
}

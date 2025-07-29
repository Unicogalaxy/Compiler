(* RISC-V寄存器和指令定义*)
type reg = | Zero | RA | SP | GP | TP | T0 | T1 | T2 | S0 | S1 | A0 | A1 | A2 | A3
           | A4 | A5 | A6 | A7 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11
           | T3 | T4 | T5 | T6

let string_of_reg = function
  | Zero -> "zero" | RA -> "ra" | SP -> "sp" | GP -> "gp" | TP -> "tp" | T0 -> "t0"
  | T1 -> "t1" | T2 -> "t2" | S0 -> "s0" | S1 -> "s1" | A0 -> "a0" | A1 -> "a1"
  | A2 -> "a2" | A3 -> "a3" | A4 -> "a4" | A5 -> "a5" | A6 -> "a6" | A7 -> "a7"
  | S2 -> "s2" | S3 -> "s3" | S4 -> "s4" | S5 -> "s5" | S6 -> "s6" | S7 -> "s7"
  | S8 -> "s8" | S9 -> "s9" | S10 -> "s10" | S11 -> "s11" | T3 -> "t3" | T4 -> "t4"
  | T5 -> "t5" | T6 -> "t6"

let argument_regs = [A0; A1; A2; A3; A4; A5; A6; A7]
let callee_saved_regs = [S1; S2; S3; S4; S5; S6; S7; S8; S9; S10; S11]
let caller_saved_temps = [T0; T1; T2; T3; T4; T5; T6]

(* ====================寄存器定义 =====================
        Zero 永远返回零      A0-A7 函数参数寄存器
        RA   返回地址        T0-T6 临时寄存器
        SP   栈指针          S0-S11 保存寄存器，存放函数的局部变量
        GP   全局指针
        TP   线程指针
 *)

type asm_instr =
  | Label of string | Directive of string | Comment of string | Li of reg * int
  | La of reg * string | Lw of reg * int * reg | Sw of reg * int * reg
  | Add of reg * reg * reg | Sub of reg * reg * reg | Mul of reg * reg * reg
  | Div of reg * reg * reg | Rem of reg * reg * reg | Slt of reg * reg * reg
  | Xori of reg * reg * int | Or of reg * reg * reg | And of reg * reg * reg
  | Beq of reg * reg * string | Bne of reg * reg * string | J of string
  | Jal of string | Jr of reg | Ecall | Addi of reg * reg * int | Mv of reg * reg

let string_of_instr = function
  | Label s -> s ^ ":" | Directive s -> "\t" ^ s | Comment s -> "\t# " ^ s
  | Li (rd, imm) -> Printf.sprintf "\tli %s, %d" (string_of_reg rd) imm
  | La (rd, label) -> Printf.sprintf "\tla %s, %s" (string_of_reg rd) label
  | Lw (rd, offset, rs) -> Printf.sprintf "\tlw %s, %d(%s)" (string_of_reg rd) offset (string_of_reg rs)
  | Sw (rs, offset, rd) -> Printf.sprintf "\tsw %s, %d(%s)" (string_of_reg rs) offset (string_of_reg rd)
  | Add (rd, rs1, rs2) -> Printf.sprintf "\tadd %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
  | Sub (rd, rs1, rs2) -> Printf.sprintf "\tsub %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
  | Mul (rd, rs1, rs2) -> Printf.sprintf "\tmul %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
  | Div (rd, rs1, rs2) -> Printf.sprintf "\tdiv %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
  | Rem (rd, rs1, rs2) -> Printf.sprintf "\trem %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
  | Slt (rd, rs1, rs2) -> Printf.sprintf "\tslt %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
  | Xori (rd, rs, imm) -> Printf.sprintf "\txori %s, %s, %d" (string_of_reg rd) (string_of_reg rs) imm
  | Or (rd, rs1, rs2) -> Printf.sprintf "\tor %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
  | And (rd, rs1, rs2) -> Printf.sprintf "\tand %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
  | Beq (rs1, rs2, label) -> Printf.sprintf "\tbeq %s, %s, %s" (string_of_reg rs1) (string_of_reg rs2) label
  | Bne (rs1, rs2, label) -> Printf.sprintf "\tbne %s, %s, %s" (string_of_reg rs1) (string_of_reg rs2) label
  | J label -> Printf.sprintf "\tj %s" label
  | Jal label -> Printf.sprintf "\tjal %s" label
  | Jr rs -> Printf.sprintf "\tjr %s" (string_of_reg rs)
  | Ecall -> "\tecall"
  | Addi (rd, rs, imm) -> Printf.sprintf "\taddi %s, %s, %d" (string_of_reg rd) (string_of_reg rs) imm
  | Mv (rd, rs) -> Printf.sprintf "\tmv %s, %s" (string_of_reg rd) (string_of_reg rs)

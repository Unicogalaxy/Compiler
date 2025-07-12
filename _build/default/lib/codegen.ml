(* lib/codegen.ml *)
open Ast

(* RISC-V寄存器定义 (无变动) *)
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
let caller_saved_regs = [RA; T0; T1; T2; T3; T4; T5; T6] @ argument_regs
let callee_saved_regs = [S0; S1; S2; S3; S4; S5; S6; S7; S8; S9; S10; S11]

(* 汇编指令 *)
type asm_instr =
  | Label of string
  | Directive of string
  | Comment of string
  | Li of reg * int
  | La of reg * string
  | Lw of reg * int * reg
  | Sw of reg * int * reg
  | Add of reg * reg * reg
  | Sub of reg * reg * reg
  | Mul of reg * reg * reg
  | Div of reg * reg * reg
  | Mod of reg * reg * reg
  | Slt of reg * reg * reg
  | Xori of reg * reg * int
  | Or of reg * reg * reg
  | And of reg * reg * reg
  | Beq of reg * reg * string
  | Bne of reg * reg * string
  | J of string
  | Jal of string
  | Jr of reg
  | Ecall
  | Addi of reg * reg * int
  | Mv of reg * reg

(* 汇编指令字符串表示 *)
let string_of_instr = function
  | Label s -> s ^ ":"
  | Directive s -> "\t" ^ s
  | Comment s -> "\t# " ^ s
  | Li (rd, imm) -> Printf.sprintf "\tli %s, %d" (string_of_reg rd) imm
  | La (rd, label) -> Printf.sprintf "\tla %s, %s" (string_of_reg rd) label
  | Lw (rd, offset, rs) -> Printf.sprintf "\tlw %s, %d(%s)" (string_of_reg rd) offset (string_of_reg rs)
  | Sw (rs, offset, rd) -> Printf.sprintf "\tsw %s, %d(%s)" (string_of_reg rs) offset (string_of_reg rd)
  | Add (rd, rs1, rs2) -> Printf.sprintf "\tadd %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
  | Sub (rd, rs1, rs2) -> Printf.sprintf "\tsub %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
  | Mul (rd, rs1, rs2) -> Printf.sprintf "\tmul %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
  | Div (rd, rs1, rs2) -> Printf.sprintf "\tdiv %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
  | Mod (rd, rs1, rs2) -> Printf.sprintf "\trem %s, %s, %s" (string_of_reg rd) (string_of_reg rs1) (string_of_reg rs2)
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

(* 代码生成环境 *)
type codegen_env = {
  mutable instructions: asm_instr list;
  mutable label_counter: int;
  var_map: (string, int) Hashtbl.t;
  mutable stack_offset: int;
  mutable current_func_name: string;
}

let create_env () = {
  instructions = [];
  label_counter = 0;
  var_map = Hashtbl.create 50;
  stack_offset = 0;
  current_func_name = "";
}

(* CORRECTED: Added the missing function definition for gen_label. *)
let gen_label env prefix =
  let count = env.label_counter in
  env.label_counter <- env.label_counter + 1;
  prefix ^ "_" ^ string_of_int count

let add_instr env instr =
  env.instructions <- instr :: env.instructions

let rec count_vars stmt =
  match stmt with
  | VarDef _ -> 1
  | Block stmts -> List.fold_left (fun acc s -> acc + count_vars s) 0 stmts
  | If (_, t, Some e) -> count_vars t + count_vars e
  | If (_, t, None) -> count_vars t
  | While (_, b) -> count_vars b
  | _ -> 0

let rec gen_expr env expr target_reg =
  match expr with
  | Constant n -> add_instr env (Li (target_reg, n))
  | Var name ->
      let offset = Hashtbl.find env.var_map name in
      add_instr env (Lw (target_reg, offset, SP))
  | UnaOp (op, e) ->
      gen_expr env e target_reg;
      (match op with
       | Neg -> add_instr env (Sub (target_reg, Zero, target_reg))
       | Not -> add_instr env (Slt (target_reg, Zero, target_reg))
       | Pos -> ())
  | BinOp (e1, op, e2) ->
      add_instr env (Addi (SP, SP, -4));
      add_instr env (Sw (T0, 0, SP));
      gen_expr env e2 T1;
      add_instr env (Lw (T0, 0, SP));
      add_instr env (Addi (SP, SP, 4));
      add_instr env (Addi (SP, SP, -4));
      add_instr env (Sw (T1, 0, SP));
      gen_expr env e1 T0;
      add_instr env (Lw (T1, 0, SP));
      add_instr env (Addi (SP, SP, 4));
      (match op with
       | Add -> add_instr env (Add (target_reg, T0, T1))
       | Sub -> add_instr env (Sub (target_reg, T0, T1))
       | Mul -> add_instr env (Mul (target_reg, T0, T1))
       | Div -> add_instr env (Div (target_reg, T0, T1))
       | Mod -> add_instr env (Mod (target_reg, T0, T1))
       | Lt  -> add_instr env (Slt (target_reg, T0, T1))
       | Gt  -> add_instr env (Slt (target_reg, T1, T0))
       | Lte -> add_instr env (Slt (target_reg, T1, T0)); add_instr env (Xori (target_reg, target_reg, 1))
       | Gte -> add_instr env (Slt (target_reg, T0, T1)); add_instr env (Xori (target_reg, target_reg, 1))
       | Eq  -> add_instr env (Sub (target_reg, T0, T1)); add_instr env (Slt (target_reg, Zero, target_reg)); add_instr env (Xori(target_reg, target_reg, 1))
       | Neq -> add_instr env (Sub (target_reg, T0, T1)); add_instr env (Slt (target_reg, Zero, target_reg))
       | And -> add_instr env (Slt (T0, Zero, T0)); add_instr env (Slt (T1, Zero, T1)); add_instr env (And (target_reg, T0, T1))
       | Or  -> add_instr env (Or (T0, T0, T1)); add_instr env (Slt (target_reg, Zero, T0)))
  | Call (fname, args) ->
      let regs_to_save = caller_saved_regs in
      List.iter (fun reg -> add_instr env (Addi (SP, SP, -4)); add_instr env (Sw (reg, 0, SP))) regs_to_save;
      List.iteri (fun i arg ->
        if i < 8 then gen_expr env arg (List.nth argument_regs i) else failwith "Too many arguments"
      ) args;
      add_instr env (Jal fname);
      if target_reg <> A0 then add_instr env (Mv (target_reg, A0));
      List.iter (fun reg -> add_instr env (Lw (reg, 0, SP)); add_instr env (Addi (SP, SP, 4))) (List.rev regs_to_save)

let rec gen_stmt env stmt =
  match stmt with
  | Empty -> ()
  | ExprStmt e -> gen_expr env e T0
  | VarDef (_, name, init) ->
      gen_expr env init T0;
      let offset = Hashtbl.find env.var_map name in
      add_instr env (Sw (T0, offset, SP))
  | Assign (name, expr) ->
      gen_expr env expr T0;
      let offset = Hashtbl.find env.var_map name in
      add_instr env (Sw (T0, offset, SP))
  | If (cond, then_stmt, else_opt) ->
      let else_label = gen_label env "else" in
      let end_label = gen_label env "endif" in
      gen_expr env cond T0;
      (match else_opt with
       | Some _ -> add_instr env (Beq (T0, Zero, else_label))
       | None -> add_instr env (Beq (T0, Zero, end_label)));
      gen_stmt env then_stmt;
      add_instr env (J end_label);
      (match else_opt with
       | Some else_stmt -> add_instr env (Label else_label); gen_stmt env else_stmt
       | None -> ());
      add_instr env (Label end_label)
  | While (cond, body) ->
      let loop_label = gen_label env "while_loop" in
      let end_label = gen_label env "while_end" in
      add_instr env (Label loop_label);
      gen_expr env cond T0;
      add_instr env (Beq (T0, Zero, end_label));
      gen_stmt env body;
      add_instr env (J loop_label);
      add_instr env (Label end_label)
  | Return expr_opt ->
      (match expr_opt with
       | Some expr -> gen_expr env expr A0
       | None -> ());
      add_instr env (J (env.current_func_name ^ "_return"))
  | Break -> ()
  | Continue -> ()
  | Block stmts -> List.iter (gen_stmt env) stmts

let gen_func env func =
  env.current_func_name <- func.fname;
  Hashtbl.clear env.var_map;
  env.stack_offset <- 0;

  let num_locals = count_vars func.body in
  let frame_size = 4 * (1 + List.length callee_saved_regs + num_locals + List.length func.params) in
  
  add_instr env (Label func.fname);
  add_instr env (Addi (SP, SP, -frame_size));
  add_instr env (Sw (RA, frame_size - 4, SP));
  List.iteri (fun i reg -> add_instr env (Sw (reg, frame_size - 8 - i * 4, SP))) callee_saved_regs;
  
  env.stack_offset <- frame_size - 8 - (List.length callee_saved_regs * 4);

  List.iteri (fun i (_, pname) ->
    if i < 8 then
      begin
        env.stack_offset <- env.stack_offset - 4;
        let offset = env.stack_offset in
        Hashtbl.add env.var_map pname offset;
        add_instr env (Sw (List.nth argument_regs i, offset, SP))
      end
  ) func.params;
  
  let rec alloc_locals stmt =
    match stmt with
    | VarDef (_, name, _) ->
        env.stack_offset <- env.stack_offset - 4;
        let offset = env.stack_offset in
        Hashtbl.add env.var_map name offset
    | Block stmts -> List.iter alloc_locals stmts
    | If (_, t, Some e) -> alloc_locals t; alloc_locals e
    | If (_, t, None) -> alloc_locals t
    | While (_, b) -> alloc_locals b
    | _ -> ()
  in
  alloc_locals func.body;
  gen_stmt env func.body;
  add_instr env (Label (func.fname ^ "_return"));
  List.iteri (fun i reg -> add_instr env (Lw (reg, frame_size - 8 - i * 4, SP))) callee_saved_regs;
  add_instr env (Lw (RA, frame_size - 4, SP));
  add_instr env (Addi (SP, SP, frame_size));
  add_instr env (Jr RA)

let gen_program program =
  let env = create_env () in
  add_instr env (Directive ".text");
  add_instr env (Directive ".globl _start");
  add_instr env (Label "_start");
  add_instr env (Jal "main");
  add_instr env (Li (A7, 93));
  add_instr env (Ecall);
  List.iter (gen_func env) program;
  List.rev env.instructions

let instructions_to_string instrs =
  String.concat "\n" (List.map string_of_instr instrs)

let codegen_program program =
  let instrs = gen_program program in
  instructions_to_string instrs
(* file: lib/codegen.ml *)
open Ast
open Analyzer
open AnnotatedAst
open Reg

(* 代码生成环境 *)
type codegen_env = {
  mutable instructions: asm_instr list;
  mutable label_counter: int;
  mutable temp_offset: int; 
  current_func_name: string;
  func_frame_size: int;
}

(* 增加loop context栈 *)
type loop_context = {
  break_label: string;
  continue_label: string;
}

let loop_stack : loop_context list ref = ref []

let gen_label env prefix =
  let count = env.label_counter in
  env.label_counter <- env.label_counter + 1;
  prefix ^ "_" ^ string_of_int count

let add_instr env instr =
  env.instructions <- instr :: env.instructions

(* 接收 AnnotatedAst.expr *)
let rec gen_expr env (aexpr: AnnotatedAst.expr) target_reg =
  match aexpr.a_loc with
  | AConstant n -> add_instr env (Li (target_reg, n))
  | AVar (_, entry) ->
      (* 不需要查找符号表！直接从注解中获取地址 *)
      add_instr env (Lw (target_reg, entry.sym_offset, S0))
  | AUnaOp (op, ae) ->
      gen_expr env ae target_reg;
      (match op with
       | Neg -> add_instr env (Sub (target_reg, Zero, target_reg))
       | Not -> add_instr env (Slt (target_reg, Zero, target_reg)); add_instr env (Xori(target_reg, target_reg, 1))
       | Pos -> ())
  | ABinOp (ae1, op, ae2) -> 
      gen_expr env ae1 T0;
      env.temp_offset <- env.temp_offset - 4;
      add_instr env (Addi (SP, SP, -4));
      let temp_addr = env.temp_offset in
      add_instr env (Sw (T0, temp_addr, S0));
      gen_expr env ae2 T1;
      add_instr env (Lw (T0, temp_addr, S0));
      env.temp_offset <- env.temp_offset + 4;
      add_instr env (Addi (SP, SP, 4));
    (match op with
    (* 算术运算 *)
    | Add -> add_instr env (Add (target_reg, T0, T1))
    | Sub -> add_instr env (Sub (target_reg, T0, T1))
    | Mul -> add_instr env (Mul (target_reg, T0, T1))
    | Div -> add_instr env (Div (target_reg, T0, T1));
    | Mod -> add_instr env (Rem (target_reg, T0, T1))
  
    (* 比较运算 - 结果为 1 (true) 或 0 (false) *)
    | Lt  -> add_instr env (Slt (target_reg, T0, T1))
    | Gt  -> add_instr env (Slt (target_reg, T1, T0))
    | Lte -> add_instr env (Slt (target_reg, T1, T0)); add_instr env (Xori (target_reg, target_reg, 1))
    | Gte -> add_instr env (Slt (target_reg, T0, T1)); add_instr env (Xori (target_reg, target_reg, 1))
    | Eq  ->
      add_instr env (Sub (target_reg, T0, T1));
      add_instr env (Xori (target_reg, target_reg, 0));  (* 清除符号位影响 *)
      let label_true = gen_label env "eq_true" in
      let label_end = gen_label env "eq_end" in
      add_instr env (Beq (target_reg, Zero, label_true));
      add_instr env (Li (target_reg, 0));
      add_instr env (J label_end);
      add_instr env (Label label_true);
      add_instr env (Li (target_reg, 1));
      add_instr env (Label label_end)

      | Neq ->
          add_instr env (Sub (target_reg, T0, T1));
          let label_true = gen_label env "neq_true" in
          let label_end = gen_label env "neq_end" in
          add_instr env (Bne (target_reg, Zero, label_true));
          add_instr env (Li (target_reg, 0));
          add_instr env (J label_end);
          add_instr env (Label label_true);
          add_instr env (Li (target_reg, 1));
          add_instr env (Label label_end)

    (* 逻辑运算 - 实现短路求值 *)
    | And ->
          let label_false = gen_label env "and_false" in
          let label_end = gen_label env "and_end" in
          add_instr env (Beq (T0, Zero, label_false));
          add_instr env (Beq (T1, Zero, label_false));
          add_instr env (Li (target_reg, 1));
          add_instr env (J label_end);
          add_instr env (Label label_false);
          add_instr env (Li (target_reg, 0));
          add_instr env (Label label_end)

      | Or ->
          let label_true = gen_label env "or_true" in
          let label_end = gen_label env "or_end" in
          add_instr env (Bne (T0, Zero, label_true));
          add_instr env (Bne (T1, Zero, label_true));
          add_instr env (Li (target_reg, 0));
          add_instr env (J label_end);
          add_instr env (Label label_true);
          add_instr env (Li (target_reg, 1));
          add_instr env (Label label_end)
    )

  | ACall (fname, a_args) ->

      let num_args = List.length a_args in

      (* --- 步骤 1: 保存调用者需要保护的临时寄存器 (t0-t6) --- *)
      List.iter (fun reg ->
        add_instr env (Addi (SP, SP, -4));
        add_instr env (Sw (reg, 0, SP));
      ) caller_saved_temps;

      (* --- 步骤 2A: 评估所有参数，并将结果暂存到栈上 --- *)
      List.iter (fun arg_expr ->
        gen_expr env arg_expr T0; (* 计算结果统一放入 t0 *)
        add_instr env (Addi (SP, SP, -4));
        add_instr env (Sw (T0, 0, SP));
      ) a_args;

      (* --- 步骤 2B: 按调用约定，将参数从暂存区加载到最终位置 --- *)
      List.iteri (fun i _ ->
        if i < List.length argument_regs then
          let reg_a = List.nth argument_regs i in
          (* ABI规定栈上传参从右到左，所以加载时偏移量要反向计算 *)
          let stack_offset = (num_args - 1 - i) * 4 in
          add_instr env (Lw (reg_a, stack_offset, SP));
      ) a_args;
      
      (* --- 步骤 3: 执行函数调用 --- *)
      add_instr env (Jal fname);

      (* --- 步骤 4: 清理整个参数区域 --- *)
      let arg_space = num_args * 4 in
      if arg_space > 0 then
        add_instr env (Addi (SP, SP, arg_space));

      (* --- 步骤 5: 以后进先出(LIFO)的顺序，恢复临时寄存器 --- *)
      let reversed_caller_saved = List.rev caller_saved_temps in
      List.iter (fun reg ->
          add_instr env (Lw (reg, 0, SP));
          add_instr env (Addi (SP, SP, 4));
      ) reversed_caller_saved;
      
      (* --- 步骤 6: 处理返回值 (总是在 a0 中) --- *)
      match aexpr.a_etype with 
      | TInt -> if target_reg <> A0 then
                  add_instr env (Mv (target_reg, A0))
                else ()
      | TVoid -> ()
      
(* 接收 AnnotatedAst.stmt *)
and gen_stmt env (astmt: AnnotatedAst.stmt) =
  match astmt with
  | AEmpty -> ()
  | AExprStmt ae -> ignore (gen_expr env ae T0)
  | AVarDef (entry, a_init) ->
      (* 变量的地址已在 entry.sym_offset 中，只需生成初始化代码 *)
      gen_expr env a_init T0;
      add_instr env (Sw (T0, entry.sym_offset, S0));
  | AAssign (entry, ae) ->
      (* 赋值操作同样直接使用注解中的地址 *)
      gen_expr env ae T0;
      add_instr env (Sw (T0, entry.sym_offset, S0))
  | AIf (a_cond, a_then, a_else_opt) ->
      let else_label = gen_label env "else" in
      let end_label = gen_label env "endif" in
      gen_expr env a_cond T0;
      (match a_else_opt with
       | Some _ -> add_instr env (Beq (T0, Zero, else_label))
       | None -> add_instr env (Beq (T0, Zero, end_label)));
      gen_stmt env a_then;
      add_instr env (J end_label);
      (match a_else_opt with
       | Some a_else -> add_instr env (Label else_label); gen_stmt env a_else
       | None -> ());
      add_instr env (Label end_label)
  | AWhile (a_cond, a_body) ->
      let loop_label = gen_label env "while_loop" in
      let continue_label = gen_label env "while_continue" in (* continue 跳转到这里 *)
      let end_label = gen_label env "while_end" in       (* break 跳转到这里 *)
      
      (* 【核心】进入新循环，将上下文压栈 *)
      let ctx = { break_label = end_label; continue_label = continue_label } in
      loop_stack := ctx :: !loop_stack;

      add_instr env (Label loop_label);
      add_instr env (Label continue_label); (* continue 的目标 *)
      gen_expr env a_cond T0;
      add_instr env (Beq (T0, Zero, end_label));
      gen_stmt env a_body;
      add_instr env (J loop_label); (* 循环体结束后跳回循环开始处 *)
      add_instr env (Label end_label); (* break 的目标 *)

      (* 【核心】离开当前循环，将上下文出栈 *)
      loop_stack := List.tl !loop_stack
  | AReturn a_expr_opt ->
      (match a_expr_opt with
       | Some ae -> gen_expr env ae A0
       | None -> ());
      add_instr env (J (env.current_func_name ^ "_return"))
  | ABreak ->
      (match !loop_stack with
       | ctx :: _ -> 
           (* 跳转到当前最内层循环的结束标签 *)
           add_instr env (J ctx.break_label)
       | [] -> failwith "Compiler error: Break used outside of a loop. This should have been caught by the analyzer.")
  | AContinue ->
      (match !loop_stack with
       | ctx :: _ -> 
           (* 跳转到当前最内层循环的继续标签 (下一次判断) *)
           add_instr env (J ctx.continue_label)
       | [] -> failwith "Compiler error: Continue used outside of a loop. This should have been caught by the analyzer.")
   | ABlock a_stmts ->
      (* 作用域问题已在第二趟解决 *)
      List.iter (gen_stmt env) a_stmts

(* 栈帧布局说明：

栈从高地址向低地址：

    [SP+frame_size] -> | return address (RA)     | <- offset = frame_size - 4
                       | old frame pointer (S0)  | <- offset = frame_size - 8
                       | callee-saved registers  | <- offset: ... -12, -16, ...
                       | spilled params (>8th)   | <- offset: 0, 4, 8, ...
                       | local variables         |
                       | temporary variables     |
                 SP -> |

*)

(* 接收 AnnotatedAst.func_def *)
let gen_func env (afunc: AnnotatedAst.func_def) =
  (* 创建一个针对此函数的、局部的代码生成环境 *)
  let func_env = {
    instructions = [];
    label_counter = env.label_counter; (* 继承全局标签计数器 *)
    temp_offset = -afunc.frame_size; (* 从分析器计算好的基准开始 *)
    current_func_name = afunc.a_fname;
    func_frame_size = afunc.frame_size;
  } in
 
  add_instr func_env (Comment ("---------frame_size:"^ (string_of_int func_env.func_frame_size) ^ "--------"));
  add_instr func_env (Comment ("---------temp_offset:" ^ (string_of_int func_env.temp_offset)^ "---------"));
  (* 1. 函数序言 - 直接使用分析器计算好的栈帧大小 *)
  add_instr func_env (Label afunc.a_fname);
  add_instr func_env (Addi (SP, SP, -afunc.frame_size));
  add_instr func_env (Sw (RA, afunc.frame_size - 4, SP));
  add_instr func_env (Sw (S0, afunc.frame_size - 8, SP));
  add_instr func_env (Addi (S0, SP, afunc.frame_size));

  (* 保存局部变量 -- 调用者策略 *)
  List.iteri (fun i reg ->
    add_instr func_env (Sw (reg, -12 - (i * 4), S0))
  ) callee_saved_regs;

  add_instr func_env (Comment ("------------好戏现在开始-----------"));

  (* 2. 将传入的参数从 a0-a7 寄存器存入它们在栈帧中的指定位置 *)
  let params_length = List.length afunc.a_params in 
    let offset = afunc.frame_size in 
      List.iteri (fun i (param_entry: Analyzer.symbol_entry) ->
        if i < List.length argument_regs then
          (* 直接从 param_entry 中获取地址偏移量，不再需要查找！*)
          add_instr func_env (Sw (List.nth argument_regs i, param_entry.sym_offset, S0))
        else
          (* 参数超过8个 *)
          let cross_offset = offset + 4 * (params_length - i - 1) in (* 注意i是从0开始的 *)
          add_instr func_env (Lw (T2, cross_offset, SP)); (* 利用T2进行debug *)
          add_instr func_env (Sw (T2, param_entry.sym_offset, S0));
      ) afunc.a_params;

  (* 3. 生成函数体代码 *)
  gen_stmt func_env afunc.a_body;

  (* 4. 函数尾声 *)
  add_instr func_env (Label (afunc.a_fname ^ "_return"));
  List.iteri (fun i reg ->
    add_instr func_env (Lw (reg, -12 - (i * 4), S0))
  ) callee_saved_regs;
  add_instr func_env (Lw (RA, afunc.frame_size - 4, SP));
  add_instr func_env (Lw (S0, afunc.frame_size - 8, SP));
  add_instr func_env (Addi (SP, SP, afunc.frame_size));
  add_instr func_env (Jr RA);
  
  (* 返回为此函数生成的所有指令，并更新全局标签计数器 *)
  env.label_counter <- func_env.label_counter;
  List.rev func_env.instructions

(* 现在接收 AnnotatedAst.program *)
let codegen_program (aprogram: AnnotatedAst.program) =
  let env = {
    instructions = [];
    label_counter = 0;
    temp_offset = 0; (* 全局临时偏移，虽然不太会用到 *)
    current_func_name = "";
    func_frame_size = 0;
  } in

  (* 添加 .text 和 .globl main 指令 *)
  add_instr env (Directive ".text");
  add_instr env (Directive ".globl main");

  (* 遍历所有注解过的函数，为它们生成代码，并拼接指令列表 *)
  let all_instrs = List.concat (List.map (gen_func env) aprogram) in
  
  (* 将所有指令转换成字符串 *)
  String.concat "\n" (List.map string_of_instr (List.rev env.instructions @ all_instrs))

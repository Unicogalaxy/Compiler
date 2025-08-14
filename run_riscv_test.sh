#!/bin/bash

# ANSI Color Codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color


# 工具路径
TOOLCHAIN_PATH="/opt/riscv"
COMPILER="${TOOLCHAIN_PATH}/bin/riscv32-unknown-elf-gcc"
EMULATOR="qemu-riscv32"

# 检查工具是否存在
if [ ! -f "$COMPILER" ]; then
    echo -e "${RED}错误：在 ${TOOLCHAIN_PATH} 中找不到 RISC-V 编译器。${NC}"
    echo "请确认您已经将下载的 'riscv32-elf-...' 工具链解压到了该路径。"
    exit 1
fi
echo -e "${GREEN}测试将使用位于 ${TOOLCHAIN_PATH} 的独立工具链。${NC}"



COMPILER_FLAGS=" --sysroot=${TOOLCHAIN_PATH}"
COMPILER_EXE="./_build/default/bin/main.exe"

# --- Helper Function (函数体无需修改) ---
run_test() {
    local test_file_path="$1"
    local expected_code="$2"
    
    local base_name=$(basename "$test_file_path" .tc)
    local asm_file="${base_name}.s"
    local exec_file="${base_name}"

    echo -e "\n--- 测试 ${YELLOW}${test_file_path}${NC} ---"

    # 1. 用您的编译器将 .tc 编译为 .s
    echo "1. 生成 RISC-V 汇编代码..."
    if ! "$COMPILER_EXE" < "$test_file_path" > ../results/asm_files/"$asm_file"; then
        echo -e "${RED}失败：您的编译器崩溃或产生错误。${NC}"
        return
    fi
    echo "   已生成 ${asm_file}"

    # 2. 使用独立的 GCC 进行汇编和标准链接
    echo "2. 使用独立GCC进行汇编 (不使用 -nostdlib)..."
    "$COMPILER" $COMPILER_FLAGS -o ../results/exec_files/"$exec_file" ../results/asm_files/"$asm_file"
    if [ $? -ne 0 ]; then
        echo -e "${RED}失败：RISC-V GCC 无法汇编或链接代码。${NC}"
        echo "   请检查工具链路径是否正确，以及生成的 .s 文件是否只包含一个全局的 'main' 函数。"
        return
    fi
    echo "   已生成可执行文件 ${exec_file}"

    # 3. 在 QEMU 中运行
    echo "3. 使用 ${EMULATOR} 运行..."
    "$EMULATOR" "../results/exec_files/${exec_file}"
    actual_code=$?

    # 4. 验证退出码
    echo "4. 验证退出码..."
    echo "   预期: ${expected_code}"
    echo "   实际: ${actual_code}"

    if [ "$actual_code" -eq "$expected_code" ]; then
        echo -e "   结果: ${GREEN}通过${NC}"
    else
        echo -e "   结果: ${RED}失败${NC}"
    fi
}

# --- 主执行逻辑 ---
echo "使用 Dune 构建您的编译器..."
if ! dune build; then
    echo -e "${RED}编译器构建失败，测试中止。${NC}"
    exit 1
fi

# 运行您的所有测试
# run_test "test_fib.tc" 55
# run_test "test_logic.tc" 42
run_test "../compiler_inputs_advanced/test_comment.tc" 1
run_test "../compiler_inputs_advanced/01_minimal.tc" 0
run_test "../compiler_inputs_advanced/02_assignment.tc" 3
run_test "../compiler_inputs_advanced/04_while_break.tc" 5
run_test "../compiler_inputs_advanced/05_function_call.tc" 7
run_test "../compiler_inputs_advanced/06_continue.tc" 4
run_test "../compiler_inputs_advanced/07_scope_shadow.tc" 1
run_test "../compiler_inputs_advanced/09_recursion.tc" 120
run_test "../compiler_inputs_advanced/10_void_fn.tc" 0
run_test "../compiler_inputs_advanced/11_precedence.tc" 14
run_test "../compiler_inputs_advanced/12_division_check.tc" 2
run_test "../compiler_inputs_advanced/13_scope_block.tc" 8
run_test "../compiler_inputs_advanced/14_nested_if_while.tc" 6
run_test "../compiler_inputs_advanced/15_multiple_return_paths.tc" 1
run_test "../compiler_inputs_advanced/16_complex_syntax.tc" 0
run_test "../compiler_inputs_advanced/16_simple_syntax.tc" 0
run_test "../compiler_inputs_advanced/17_complex_expressions.tc" 159
run_test "../compiler_inputs_advanced/18_many_variables.tc" 133
run_test "../compiler_inputs_advanced/19_many_arguments.tc" 124
run_test "../compiler_inputs_advanced/20_comprehensive.tc" 104



echo -e "\n--- 所有测试已完成 ---"
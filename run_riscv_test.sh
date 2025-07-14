#!/bin/bash

# ANSI Color Codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# --- 【核心修改 1】: 指向您下载的、独立的工具链 ---
# 假设您已经将下载的工具链解压到了 /opt/riscv 目录
# 请根据您实际的解压路径修改此变量
TOOLCHAIN_PATH="/opt/riscv"
COMPILER="${TOOLCHAIN_PATH}/bin/riscv64-unknown-elf-gcc"
EMULATOR="qemu-riscv32"

# 检查工具是否存在
if [ ! -f "$COMPILER" ]; then
    echo -e "${RED}错误：在 ${TOOLCHAIN_PATH} 中找不到 RISC-V 编译器。${NC}"
    echo "请确认您已经将下载的 'riscv32-elf-...' 工具链解压到了该路径。"
    exit 1
fi
echo -e "${GREEN}测试将使用位于 ${TOOLCHAIN_PATH} 的独立工具链。${NC}"


# --- 【核心修改 2】: 使用 --sysroot 标志进行标准链接 ---
# 这是解决 "cannot find crt0.o" 问题的最可靠方法。
# 它告诉编译器将指定的工具链目录作为自己的“系统根”，从而在其中查找所有库文件。
# 注意：我们不使用 -nostdlib。
COMPILER_FLAGS="-march=rv32i -mabi=ilp32 --sysroot=${TOOLCHAIN_PATH}"
COMPILER_EXE="./_build/default/main.exe"

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
    if ! "$COMPILER_EXE" < "$test_file_path" > "$asm_file"; then
        echo -e "${RED}失败：您的编译器崩溃或产生错误。${NC}"
        return
    fi
    echo "   已生成 ${asm_file}"

    # 2. 使用独立的 GCC 进行汇编和标准链接
    echo "2. 使用独立GCC进行汇编 (不使用 -nostdlib)..."
    "$COMPILER" $COMPILER_FLAGS -o "$exec_file" "$asm_file"
    if [ $? -ne 0 ]; then
        echo -e "${RED}失败：RISC-V GCC 无法汇编或链接代码。${NC}"
        echo "   请检查工具链路径是否正确，以及生成的 .s 文件是否只包含一个全局的 'main' 函数。"
        return
    fi
    echo "   已生成可执行文件 ${exec_file}"

    # 3. 在 QEMU 中运行
    echo "3. 使用 ${EMULATOR} 运行..."
    "$EMULATOR" "./${exec_file}"
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
run_test "test_fib.tc" 55
run_test "test_logic.tc" 42
run_test "../compiler_inputs_advanced/01_minimal.tc" 0
run_test "../compiler_inputs_advanced/02_assignment.tc" 3

echo -e "\n--- 所有测试已完成 ---"
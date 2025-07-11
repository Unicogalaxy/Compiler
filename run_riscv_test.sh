#!/bin/bash

# ANSI Color Codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# --- Configuration ---
# 检查是否存在64位或32位工具链
if command -v riscv64-unknown-elf-gcc &> /dev/null && command -v qemu-riscv64 &> /dev/null; then
    COMPILER="riscv64-unknown-elf-gcc"
    EMULATOR="qemu-riscv64"
elif command -v riscv32-unknown-elf-gcc &> /dev/null && command -v qemu-riscv32 &> /dev/null; then
    COMPILER="riscv32-unknown-elf-gcc"
    EMULATOR="qemu-riscv32"
else
    echo -e "${RED}Error: RISC-V toolchain (GCC and QEMU) not found.${NC}"
    echo "Please install 'riscv64-unknown-elf-gcc' and 'qemu-riscv64' or their 32-bit equivalents."
    exit 1
fi

COMPILER_FLAGS="-march=rv32i -mabi=ilp32 -static -nostdlib" 
COMPILER_EXE="./_build/default/main.exe"

# --- Helper Function ---
# $1: Test file (.tc)
# $2: Expected exit code
run_test() {
    local test_file="$1"
    local expected_code="$2"
    local base_name=$(basename "$test_file" .tc)
    local asm_file="${base_name}.s"
    local exec_file="${base_name}"

    echo -e "\n--- Testing ${YELLOW}${test_file}${NC} ---"

    # 1. Compile .tc to .s using your compiler
    echo "1. Generating RISC-V assembly..."
    if ! "$COMPILER_EXE" "$test_file" > "$asm_file"; then
        echo -e "${RED}FAIL: Compiler crashed or produced an error.${NC}"
        return
    fi
    # Check if assembly file is empty
    if [ ! -s "$asm_file" ]; then
        echo -e "${RED}FAIL: Generated assembly file is empty.${NC}"
        return
    fi
    echo "   Generated ${asm_file}"

    # 2. Assemble and link the .s file using RISC-V GCC
    echo "2. Assembling with ${COMPILER}..."
    "$COMPILER" $COMPILER_FLAGS -o "$exec_file" "$asm_file"
    if [ $? -ne 0 ]; then
        echo -e "${RED}FAIL: RISC-V GCC failed to assemble the code.${NC}"
        return
    fi
    echo "   Generated executable ${exec_file}"

    # 3. Run the executable in QEMU and get the exit code
    echo "3. Running with ${EMULATOR}..."
    "$EMULATOR" "$exec_file"
    actual_code=$?

    # 4. Compare actual exit code with expected exit code
    echo "4. Verifying exit code..."
    echo "   Expected: ${expected_code}"
    echo "   Actual:   ${actual_code}"

    if [ "$actual_code" -eq "$expected_code" ]; then
        echo -e "   Result: ${GREEN}PASS${NC}"
    else
        echo -e "   Result: ${RED}FAIL${NC}"
    fi
}


# --- Main Execution ---

# First, ensure the compiler is built
echo "Building the compiler with Dune..."
if ! dune build; then
    echo -e "${RED}Compiler build failed. Aborting tests.${NC}"
    exit 1
fi

# Run tests
run_test "test_fib.tc" 55
run_test "test_logic.tc" 42
# 您也可以在这里添加对项目中原有测试文件的测试
# run_test "test_codegen.tc" 8 

echo -e "\n--- All Tests Completed ---"

# --- Cleanup ---
# You can uncomment these lines to automatically remove generated files
# echo "Cleaning up generated files..."
# rm -f *.s test_fib test_logic test_codegen
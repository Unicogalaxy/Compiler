#!/bin/bash

# ANSI Color Codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# --- Configuration ---
# This version intelligently finds the best available toolchain.
# It uses the 64-bit tools because they are more commonly available,
# but it passes 32-bit flags to the compiler.

if command -v riscv64-unknown-elf-gcc &> /dev/null && command -v qemu-riscv64 &> /dev/null; then
    COMPILER="riscv64-unknown-elf-gcc"
    EMULATOR="qemu-riscv32"
    echo -e "${GREEN}Found 64-bit RISC-V toolchain. Will compile for 32-bit target.${NC}"
else
    echo -e "${RED}Error: Required tools ('riscv64-unknown-elf-gcc' and 'qemu-riscv64') not found.${NC}"
    echo "Please install them, e.g., using: sudo apt install gcc-riscv64-unknown-elf qemu-user"
    exit 1
fi

# We explicitly tell the 64-bit compiler to generate 32-bit code.
COMPILER_FLAGS="-march=rv32i -mabi=ilp32 -static -nostdlib"
COMPILER_EXE="./_build/default/main.exe"

# --- Helper Function ---
run_test() {
    local test_file_path="$1"
    local expected_code="$2"
    
    # Use basename to get the filename without directory path
    local base_name=$(basename "$test_file_path" .tc)
    local asm_file="${base_name}.s"
    local exec_file="${base_name}"

    echo -e "\n--- Testing ${YELLOW}${test_file_path}${NC} ---"

    # 1. Compile .tc to .s using your compiler
    echo "1. Generating RISC-V assembly..."
    # Use redirection to pass the file content to your compiler's stdin
    if ! "$COMPILER_EXE" < "$test_file_path" > "$asm_file"; then
        echo -e "${RED}FAIL: Your compiler crashed or produced an error.${NC}"
        return
    fi
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

    # 3. Run the executable in QEMU
    # The 64-bit QEMU can run 32-bit code when compiled with the correct flags.
    echo "3. Running with ${EMULATOR}..."
    "$EMULATOR" "./${exec_file}"
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
echo "Building the compiler with Dune..."
if ! dune build; then
    echo -e "${RED}Compiler build failed. Aborting tests.${NC}"
    exit 1
fi

# Run tests with the provided paths
# You can add the paths to your test files here
run_test "test_fib.tc" 55
run_test "test_logic.tc" 42
run_test "../compiler_inputs_advanced/01_minimal.tc" 0
run_test "../compiler_inputs_advanced/02_assignment.tc" 3


echo -e "\n--- All Tests Completed ---"
# ANSI Color Codes
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

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

run_exec(){
    echo -e "\n--- 测试 ${YELLOW}${test_file_path}${NC} ---"
    local asm_file="19_many_arguments.s"
    local exec_file="19_many_arguments"

    echo " 使用独立GCC进行汇编 (不使用 -nostdlib)..."
    "$COMPILER" $COMPILER_FLAGS -o ../results/exec_files/"$exec_file" ../results/asm_files/"$asm_file"
    if [ $? -ne 0 ]; then
        echo -e "${RED}失败：RISC-V GCC 无法汇编或链接代码。${NC}"
        echo "   请检查工具链路径是否正确，以及生成的 .s 文件是否只包含一个全局的 'main' 函数。"
        return
    fi
    echo "   已生成可执行文件 ${exec_file}"

    echo "使用 ${EMULATOR} 运行..."
    "$EMULATOR" "../results/exec_files/${exec_file}"
    actual_code=$?

    echo "退出码: ${actual_code}"
}

run_exec


#  ; === 在这里插入退出指令 ===
#     li a0, 123          ; 1. 设置退出码为 123
#     li a7, 93           ; 2. 设置 a7 为 exit 的系统调用编号 (93)
#     ecall               ; 3. 触发系统调用，程序立即终止
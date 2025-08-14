#用 QEMU 启动模拟器，然后用 GDB 连接它
#-S 让 QEMU 启动后暂停，等待 GDB 连接。
#-gdb tcp::1234 让 QEMU 在 1234 端口监听 GDB 连接。
qemu-riscv32 -g 1234 ./results/exec_files/

#另开一个终端启动 GDB
riscv32-unknown-elf-gdb ./results/exec_files/
(gdb) target remote :1234     # 连接 QEMU
(gdb) break main              # 断在 main（或 _start）
(gdb) continue                # 开始执行
(gdb) info registers t1       # 看寄存器
(gdb) x/10i $pc                # 查看指令
(gdb) si                      # 单步执行
(gdb) quit                    # 退出

monitor system_reset
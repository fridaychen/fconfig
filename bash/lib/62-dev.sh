function elf-addr2code {
    addr2line -e "$1" -f -C -p "$2"
}

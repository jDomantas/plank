const RELOCATABLE_FILE_TYPE: u16 = 1;
const EXECUTABLE_FILE_TYPE: u16 = 2;

const ELF_MACHINE_386: u16 = 3;

const ELF_CURRENT_VERSION: u32 = 1;

#[derive(Debug, Clone)]
struct ElfHeader {
    /// Elf file identifier
    ident: [u8; 16],
    /// File type, either relocatable object file or executable
    file_type: u16,
    /// Machine type
    machine: u16,
    /// Elf format version
    version: u32,
    /// Address of entry point, 0 if no associated entry point exists
    entry_address: u32,
    /// Program header table file offset, 0 if no table
    program_header_offset: u32,
    /// Section header table file offset, 0 if no table
    section_header_offset: u32,
    /// Processor specific flags
    flags: u32,
    /// Elf header size in bytes
    elf_header_size: u16,
    /// Size of one entry in program header table
    program_header_size: u16,
    /// Number of entries in program header table
    program_header_count: u16,
    /// Size of one entry in section header table
    section_header_size: u16,
    /// Number of entries in section header table
    section_header_count: u16,
    /// Index of entry in section header table that holds string table.
    /// SHN_UNDEF if string table does not exist.
    string_table_index: u16,
}

const ELF_IDENT: [u8; 16] = [
    0x7f, b'E', b'L', b'F', // EI_MAG0 to EI_MAG3
    1, // EI_CLASS, always 32-bit
    1, // EI_DATA, ELFDATA2LSB for little endian encoding
    ELF_CURRENT_VERSION, // EI_VERSION
    0, 0, 0, 0, 0, 0, 0, 0, 0, // EI_PAD, padding bytes
];

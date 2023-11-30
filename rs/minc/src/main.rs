use std::fs::File;
use std::io::Write;

mod minc_ast;
mod minc_parse;
mod minc_cogen;

/* read XML file and convert it to assembly string */
fn file_xml_to_asm(file_xml : &str) -> Result<String, minc_parse::InvalidXML> {
    let program = minc_parse::file_xml_to_ast(file_xml)?; // in minc_parse.rs
    let asm = minc_cogen::ast_to_asm_program(program); // in minc_cogen.rs
    Ok(asm)
}

/* read XML file, convert it to assembly string, and write
   it to a file (file_asm) */
fn file_xml_to_file_asm(file_xml : &str, file_asm : &str) {
    let asm = file_xml_to_asm(file_xml).unwrap();
    let mut file = File::create(file_asm).unwrap();
    file.write_all(asm.as_bytes()).unwrap();
}

/* entry point 
   ./minc fun.xml fun.s 
   read an XML file and generate assembly code in fun.s
*/
fn main() {
    let args : Vec<String> = std::env::args().collect();
    let file_xml = &args[1];
    let file_asm = &args[2];
    file_xml_to_file_asm(file_xml, file_asm)
}


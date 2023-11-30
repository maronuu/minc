/* minc_parse

   parsing actually means converting XML into the
   Abstract Syntax Tree (AST) defined in the respective
   language (see minc_ast.{ml,jl,go,rs}).
   XML is made by converting the original source (.c) file
   by parser/minc_to_xml.py.

            minc_to_xml.py
   .c file ----------------> xml file

            str_to_dom (minc_parse)
           --------------------------> xml (DOM) tree

            dom_to_ast_{type,expr,stmt,...}
            (minc_parse)
           ---------------------------------> AST

   str_to_dom : convert a string into an XML tree (called
   Document Object Model, or DOM), using an appropriate
   XML parsing library of your language.

   dom_to_ast_{type,expr,stmt,...} : convert an XML tree representing 
   type, expression, statement, etc. into the respective AST.

   str_to_ast : convert a string representing a program
   into AST, by first calling str_to_dom and then dom_to_ast_program

   file_xml_to_ast : read an XML file (get a string) and 
   convert it into AST. this is the function called from the main
   function

*/

extern crate minidom;
use minidom::Element;

use crate::minc_ast;

/* --- things related to string -> dom --- */

/* string -> dom
   <foo>bar</foo> -> Element("foo", [Text("bar")]) */
fn str_to_dom(s : &str) -> Element {
    let node : Element = s.parse().expect(&format!("invalid XML string\n{}\n", s));
    node
}

/* xml in file -> dom */
#[allow(dead_code)]
fn file_xml_to_dom(file_xml : &str) -> Element {
    let s = std::fs::read_to_string(file_xml).expect(&format!("Failed to open {}", file_xml));
    str_to_dom(&s)
}

fn dom_to_str(elem : &Element) -> String {
    format!("{:?}", elem)
}

/* --- parser (XML DOM -> Abstract Syntax Tree) ---

   dom_to_ast_xxxx converts a dom tree supposedly 
   representing xxxx into the corresponding abstract
   syntax tree (see minc_ast for its definition).

   e.g., dom_to_ast_expr converts a dom tree supposedly
   representing an expression into a data 
   representing an expression. 

*/

/* raised when the input XML DOM tree does not have the right structure */
#[derive(Debug)]
pub enum InvalidXML {
    Error(String)
}

fn invalid_xml<T>(elem : &Element) -> Result<T, InvalidXML> {
    Err(InvalidXML::Error(dom_to_str(elem)))
}

// check if this is a node like <tag/>
fn check_singleton(elem : &Element) -> Result<(), InvalidXML> {
    if elem.children().count() != 0 { invalid_xml(elem) }
    else if elem.text() != "" { invalid_xml(elem) }
    else { Ok(()) }
}

// check if this is a node like <tag>text</tag> and returns text
fn check_get_text(elem : &Element) -> Result<String, InvalidXML> {
    if elem.children().count() != 0 { invalid_xml(elem) }
    else if elem.text() == "" { invalid_xml(elem) }
    else { Ok(elem.text()) }
}

// check if this is a node like <tag>elem</tag> and return elem
fn check_get_child_1<'a>(elem : &'a Element) -> Result<&'a Element, InvalidXML> {
    let mut children = elem.children();
    let c0 = children.next().unwrap();
    match children.next() {
        None => Ok(c0),
        _ => invalid_xml(elem)
    }
}

// check if this is a node like <tag><t0>c0</t0><t1>c1</t1></tag> and return c0, c1
fn check_get_children_2<'a>(elem : &'a Element, t0 : &str, t1 : &str)
                            -> Result<(&'a Element, &'a Element), InvalidXML> {
    let mut children = elem.children();
    let c0 = children.next().unwrap();
    let c1 = children.next().unwrap();
    match children.next() {
        None => {
            if c0.name() != t0 { invalid_xml(elem) }
            else if c1.name() != t1 { invalid_xml(elem) }
            else { Ok((c0, c1)) }
        },
        _ => invalid_xml(elem)
    }
}

// check if this is a node like <tag><t0>c0</t0><t1>c1</t1><t2>c2</t2></tag> and return c0, c1, c2
fn check_get_children_3<'a>(elem : &'a Element, t0 : &str, t1 : &str, t2 : &str)
                            -> Result<(&'a Element, &'a Element, &'a Element), InvalidXML> {
    let mut children = elem.children();
    let c0 = children.next().unwrap();
    let c1 = children.next().unwrap();
    let c2 = children.next().unwrap();
    match children.next() {
        None => {
            if c0.name() != t0 { invalid_xml(elem) }
            else if c1.name() != t1 { invalid_xml(elem) }
            else if c2.name() != t2 { invalid_xml(elem) }
            else { Ok((c0, c1, c2)) }
        },
        _ => invalid_xml(elem)
    }
}

// check if this is a node like <tag><t0>c0</t0><t1>c1</t1><t2>c2</t2></tag> and return c0, c1, c2
fn check_get_children_4<'a>(elem : &'a Element, t0 : &str, t1 : &str, t2 : &str, t3 : &str)
                            -> Result<(&'a Element, &'a Element, &'a Element, &'a Element), InvalidXML> {
    let mut children = elem.children();
    let c0 = children.next().unwrap();
    let c1 = children.next().unwrap();
    let c2 = children.next().unwrap();
    let c3 = children.next().unwrap();
    match children.next() {
        None => {
            if c0.name() != t0 { invalid_xml(elem) }
            else if c1.name() != t1 { invalid_xml(elem) }
            else if c2.name() != t2 { invalid_xml(elem) }
            else if c3.name() != t3 { invalid_xml(elem) }
            else { Ok((c0, c1, c2, c3)) }
        },
        _ => invalid_xml(elem)
    }
}

/* apply f to each element of the iterator a and return the vector of the result.
   if any of f returns an error, returns an error */
fn map_array<S, T> (f : fn(S) -> Result<T, InvalidXML>, a : &mut dyn Iterator<Item=S>)
                    -> Result<Vec<T>, InvalidXML> {
    let mut b = Vec::<T>::new();
    for v in a {
        b.push(f(v)?)
    }
    Ok(b)
}

/* dom tree for type expression -> ast for type expression
   (note: only possible type expression in minc is "long".
   the framework is designed so it is easy to extend it with
   a new type) */

fn dom_to_ast_type(elem : &Element) -> Result<minc_ast::TypeExpr, InvalidXML> {
    // <primitive_type>long</primitive_type>
    match elem.name() {
	"primitive_type" => {
	    let name = check_get_text(elem)?;
	    Ok(minc_ast::TypeExpr::Primitive(name))
	},
	_ => { invalid_xml(elem) }
    }
}

/* dom tree for expression -> ast for expression */
fn dom_to_ast_expr(elem : &Element) -> Result<minc_ast::Expr, InvalidXML> {
    match elem.name() {
	"int_literal" => {	// <int_literal>123</int_literal>
	    let s = check_get_text(elem)?;
            match s.parse() {
                Ok(val) => Ok(minc_ast::Expr::IntLiteral(val)),
                Err(_) => invalid_xml(elem)
            }
	},
	"var" => {		// <var>x</var>
	    let name = check_get_text(elem)?;
	    Ok(minc_ast::Expr::Id(name.to_string()))
	},
	"un_op" => {	// <un_op><op>-</op><arg>expr</arg></un_op>
	    let (op_elem, arg_elem) = check_get_children_2(elem, "op", "arg")?;
	    let op = check_get_text(op_elem)?;
	    let arg = dom_to_ast_expr(check_get_child_1(arg_elem)?)?;
	    Ok(minc_ast::Expr::Op(op.to_string(), vec!{arg}))
	},
	"bin_op" => {	// <bin_op><op>+</op><left>expr</left><right>expr</right></bin_op>
	    let (op_elem, left_elem, right_elem) = check_get_children_3(elem, "op", "left", "right")?;
	    let op = check_get_text(op_elem)?;
	    let left = dom_to_ast_expr(check_get_child_1(left_elem)?)?;
	    let right = dom_to_ast_expr(check_get_child_1(right_elem)?)?;
	    Ok(minc_ast::Expr::Op(op.to_string(), vec!{left, right}))
	},
	"call" => {		// <call><fun>expr</fun><args>expr expr expr ...</args></call>
	    let (fun_elem, args_elem) = check_get_children_2(elem, "fun", "args")?;
	    let fun = dom_to_ast_expr(check_get_child_1(fun_elem)?)?;
            let args = map_array(dom_to_ast_expr, &mut args_elem.children())?;
	    Ok(minc_ast::Expr::Call(Box::new(fun), args))
	},
	"paren" => {	// <paren>expr</paren>
	    let expr = dom_to_ast_expr(check_get_child_1(elem)?)?;
	    Ok(minc_ast::Expr::Paren(Box::new(expr)))
	},
        _ => { invalid_xml(elem) }
    }
}

/* dom tree for variable declaration -> ast for variable declaration */
fn dom_to_ast_decl(elem : &Element) -> Result<minc_ast::Decl, InvalidXML> {
    // <decl><type>type_expr</type><name>x</name></decl>
    match elem.name() {
	"decl" => {
	    let (var_type_elem, var_name_elem) = check_get_children_2(elem, "type", "name")?;
	    let var_type = dom_to_ast_type(check_get_child_1(var_type_elem)?)?;
	    let name = check_get_text(var_name_elem)?.to_string();
	    Ok(minc_ast::Decl{var_type, name})
	},
        _ => { invalid_xml(elem) }
    }
}

/* dom tree for statement -> ast for statement */
fn dom_to_ast_stmt(elem : &Element) -> Result<minc_ast::Stmt, InvalidXML> {
    match elem.name() {
	"empty" => {            // <empty></empty>
	    check_singleton(elem)?;
	    Ok(minc_ast::Stmt::Empty)
	},
	"continue" => {         // <continue></continue>
	    check_singleton(elem)?;
	    Ok(minc_ast::Stmt::Continue)
	},
	"break" => {            // <break></break>
	    check_singleton(elem)?;
	    Ok(minc_ast::Stmt::Break)
	},
	"return" => {           // <return>expr</return>
	    let return_expr = dom_to_ast_expr(check_get_child_1(elem)?)?;
	    Ok(minc_ast::Stmt::Return(return_expr))
	},
	"expr_stmt" => {	// <expr_stmt>expr</expr_stmt>
	    let expr = dom_to_ast_expr(check_get_child_1(elem)?)?;
	    Ok(minc_ast::Stmt::Expr(expr))
	},
	"compound" => {	// <compound><decls>...</decls><stmts>...</stmts></compound>
	    let (decls_elem, stmts_elem) = check_get_children_2(elem, "decls", "stmts")?;
	    let decls = map_array(dom_to_ast_decl, &mut decls_elem.children())?;
	    let stmts = map_array(dom_to_ast_stmt, &mut stmts_elem.children())?;
	    Ok(minc_ast::Stmt::Compound(decls, stmts))
	},
	"if" => { // <if><cond>expr</cond><then>stmt</then><else>stmt</else></if>
	    let n_children = elem.children().count();
	    if n_children == 3 {
		let (cond_elem, then_elem, else_elem) = check_get_children_3(elem, "cond", "then", "else")?;
		let cond = dom_to_ast_expr(check_get_child_1(cond_elem)?)?;
		let then_stmt = dom_to_ast_stmt(check_get_child_1(then_elem)?)?;
		let else_stmt = dom_to_ast_stmt(check_get_child_1(else_elem)?)?;
		Ok(minc_ast::Stmt::If(cond, Box::new(then_stmt), Some(Box::new(else_stmt))))
	    } else if n_children == 2 {
		let (cond_elem, then_elem) = check_get_children_2(elem, "cond", "then")?;
		let cond = dom_to_ast_expr(check_get_child_1(cond_elem)?)?;
		let then_stmt = dom_to_ast_stmt(check_get_child_1(then_elem)?)?;
		Ok(minc_ast::Stmt::If(cond, Box::new(then_stmt), None))
	    } else {
	        invalid_xml(elem)
            }
	},
	"while" => { // <while><cond>expr</cond><body>stmt</body></while>
	    let (cond_elem, body_elem) = check_get_children_2(elem, "cond", "body")?;
	    let cond = dom_to_ast_expr(check_get_child_1(cond_elem)?)?;
	    let body = dom_to_ast_stmt(check_get_child_1(body_elem)?)?;
	    Ok(minc_ast::Stmt::While(cond, Box::new(body)))
	},
        _ => { invalid_xml(elem) }
    }
}

/* dom tree for a function parameter -> ast for parameter */
fn dom_to_ast_param(elem : &Element) -> Result<minc_ast::Decl, InvalidXML> {
    // <param><type>type_expr</type><name>x</name></param>
    match elem.name() {
	"param" => {
	    let (param_type_elem, param_name_elem) = check_get_children_2(elem, "type", "name")?;
	    let var_type = dom_to_ast_type(check_get_child_1(param_type_elem)?)?;
	    let name = check_get_text(param_name_elem)?.to_string();
	    Ok(minc_ast::Decl{var_type, name})
	}
	_ => { invalid_xml(elem) }
    }
}

/* dom tree for a toplevel definition -> ast for a toplevel definition
   (note: only possible toplevel definition in minc is a function definition) */
fn dom_to_ast_def(elem : &Element) -> Result<minc_ast::Def, InvalidXML> {
    match elem.name() {
	"fun_def" => {
	    // <fun_def>
	    //  <name>f</name>
	    //  <params>
	    //   <param><type>TYPE</type><name>x</name></param>
	    //   <param><type>TYPE</type><name>y</name></param>
	    //  </params>
	    //  <return_type>TYPE</return_type>
	    //  <body>STMT</body>
	    // </fun_def>
	    let (name_elem, params_elem, return_type_elem, body_elem)
                = check_get_children_4(elem, "name", "params", "return_type", "body")?;
	    let name = check_get_text(name_elem)?.to_string();
	    let params = map_array(dom_to_ast_param, &mut params_elem.children())?;
	    let return_type = dom_to_ast_type(check_get_child_1(return_type_elem)?)?;
	    let body = dom_to_ast_stmt(check_get_child_1(body_elem)?)?;
	    Ok(minc_ast::Def::Fun(name, params, return_type, body))
	}
        _ => { invalid_xml(elem) }
    }
}

/* dom tree for a program -> ast for a program */
fn dom_to_ast_program(elem : &Element) -> Result<minc_ast::Program, InvalidXML> {
    match elem.name() {
	"program" => {
	    let defs = map_array(dom_to_ast_def, &mut elem.children())?;
	    Ok(minc_ast::Program{defs})
	},
	_ => { invalid_xml(elem) }
    }
}

/* XML string -> abstract syntax tree */
fn str_to_ast(s : &str) -> Result<minc_ast::Program, InvalidXML> {
    let dom = str_to_dom(s);
    dom_to_ast_program(&dom)
}

/* XML file -> abstract syntax tree */
pub fn file_xml_to_ast(file_xml : &str) -> Result<minc_ast::Program, InvalidXML> {
    let s = std::fs::read_to_string(file_xml).unwrap();
    str_to_ast(&s)
}


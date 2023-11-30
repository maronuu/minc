#!/usr/bin/python3

"""
minc_to_xml
"""

import re

import xml.dom.minidom

def make_document():
    impl = xml.dom.minidom.getDOMImplementation()
    doc = impl.createDocument("https://root.com/", "root", None)
    return doc

doc = make_document()

def text(txt):
    return doc.createTextNode(txt)

def node(tag, children):
    elem = doc.createElement(tag)
    for child in children:
        elem.appendChild(child)
    return elem

import tatsu
import minc_grammar

class text__:
    """
    text node
    """
    def __init__(self, txt):
        self.txt = txt
    def to_str(self, depth):
        """
        -> string
        """
        indent = " " * depth
        txt = self.txt
        return f"{indent}{txt}"

class node__:
    """
    node
    """
    def __init__(self, tag, children):
        self.tag = tag
        self.children = children
    def to_str(self, depth):
        """
        -> string
        """
        children_s = "\n".join([child.to_str(depth + 1) for child in self.children])
        indent = " " * depth
        return ("{indent}<{tag}>\n{children_s}\n{indent}</{tag}>"
                .format(tag=self.tag, indent=indent, children_s=children_s))

def xml_of_ast_type(typ):
    """
    long -> <primitive_type>long</primitive_type>
    """
    return node("primitive_type", [text(typ)])

def xml_of_ast_param(param):
    """
    (long, x) ->
      <param>
       <type><primitive_type>long</primitive_type></type>
       <name>x</name>
      </param>
    """
    (param_type, param_name) = param
    return node("param", [node("type", [xml_of_ast_type(param_type)]),
                          node("name", [text(param_name)])])

def del_commas(args):
    """
    ["x", ",", "y", ",", "z"] -> ["x", "y", "z"]
    """
    return args[::2]

def xml_of_ast_expr(expr):
    """
    123 -> <int_literal>123</int_literal>
    x   -> <var>x</var>
    -x  -> <un_op>
             <op>-</op>
             <arg><var>x</var></arg>
           </un_op>
    (x) -> <paren>
             <var>x</var>
           </paren>
    x + y -> <bin_op>
               <op>+</op>
               <left><var>x</var></left>
               <right><var>y</var></right>
             </bin_op>
    f(x, y) -> <call>
                 <fun><var>f</var></fun>
                 <args><var>x</var><var>y</var></args>
               </call>
    """
    if isinstance(expr, type("")):
        if re.match(r"\d+", expr):
            return node("int_literal", [text(expr)])
        return node("var", [text(expr)])
    assert(isinstance(expr, type(()))), expr
    if len(expr) == 2:
        (un_op, sub_expr) = expr
        assert(un_op in ("+", "-", "!", "~")), expr
        return node("un_op", [node("op", [text(un_op)]),
                              node("arg", [xml_of_ast_expr(sub_expr)])])
    if len(expr) == 3:
        if expr[0] == "(":  # ( sub_expr )
            (lparen, sub_expr, rparen) = expr
            assert([lparen, rparen] == ["(", ")"]), expr
            return node("paren", [xml_of_ast_expr(sub_expr)])
        (left, bin_op, right) = expr
        assert(bin_op in ["=", "==", "!=", "<=", ">=", "<", ">",
                          "+", "-", "*", "/", "%"]), expr
        return node("bin_op", [node("op", [text(bin_op)]),
                               node("left", [xml_of_ast_expr(left)]),
                               node("right", [xml_of_ast_expr(right)])])
    if len(expr) == 4:
        (fun_expr, lparen, args, rparen) = expr
        assert([lparen, rparen] == ["(", ")"]), expr
        return node("call", [node("fun", [xml_of_ast_expr(fun_expr)]),
                             node("args", [xml_of_ast_expr(arg) for arg in del_commas(args)])])
    assert(0), expr

def xml_of_ast_decl(decl):
    """
    long x; ->
     <decl>
      <type><primitive_type>long</primitive_type></type>
      <name>x</name>
     </decl>
    """
    (var_type, var_name, semi_colon) = decl
    assert(semi_colon == ";"), decl
    return node("decl", [node("type", [xml_of_ast_type(var_type)]),
                         node("name", [text(var_name)])])

def xml_of_ast_stmt(stmt):
    """
    ;         -> <empty></empty>
    break;    -> <break></break>
    continue; -> <continue></continue>
    return x; -> <return><var>x</var></return>
    { long x; return x; } ->
       <compound>
        <decls>
         <decl>
          <type><primitive_type>long</primitive_type></type>
          <name>x</name>
         </decl>
        </decls>
        <stmts>
         <return><var>x</var></return>
        </stmts>
       </compound>
    if (x) return y; else return z; ->
       <if>
        <cond><var>x</var></cond>
        <then><return><var>y</var></return></then>
        <else><return><var>z</var></return></else>
       </if>
    while (x) return y; ->
       <while>
        <cond><var>x</var></cond>
        <body><return><var>y</var></return></body>
       </while>
    f(x, y) ->
       <expr_stmt>
        <call>
         <fun><var>f</var></fun>
         <args><var>x</var><var>y</var></args>
        </call>
       </expr_stmt>
    """
    fst = stmt[0]
    if fst == ";":
        assert(stmt == (";",)), stmt
        return node("empty", [])
    if fst == "break":
        assert(stmt == ("break", ";")), stmt
        return node("break", [])
    if fst == "continue":
        assert(stmt == ("continue", ";")), stmt
        return node("continue", [])
    if fst == "return":
        (ret, expr, semi_colon) = stmt
        assert((ret, semi_colon) == ("return", ";")), stmt
        return node("return", [xml_of_ast_expr(expr)])
    if fst == "{":
        (lbrace, decls, stmts, rbrace) = stmt
        assert((lbrace, rbrace) == ("{", "}")), stmt
        return node("compound", [node("decls", [xml_of_ast_decl(decl) for decl in decls]),
                                 node("stmts", [xml_of_ast_stmt(stmt) for stmt in stmts])])
    if fst == "if":
        if len(stmt) == 7:
            (if_, lparen, cond, rparen, then_stmt, else_, else_stmt) = stmt
        else:
            assert(len(stmt) == 5), stmt
            (if_, lparen, cond, rparen, then_stmt) = stmt
            (else_, else_stmt) = "else", None
        assert((if_, lparen, rparen, else_) == ("if", "(", ")", "else")), stmt
        if else_stmt:
            return node("if", [node("cond", [xml_of_ast_expr(cond)]),
                               node("then", [xml_of_ast_stmt(then_stmt)]),
                               node("else", [xml_of_ast_stmt(else_stmt)])])
        return node("if", [node("cond", [xml_of_ast_expr(cond)]),
                           node("then", [xml_of_ast_stmt(then_stmt)])])
    if fst == "while":
        (while_, lparen, cond, rparen, body) = stmt
        assert((while_, lparen, rparen) == ("while", "(", ")")), stmt
        return node("while", [node("cond", [xml_of_ast_expr(cond)]),
                              node("body", [xml_of_ast_stmt(body)])])
    (expr, semi_colon) = stmt
    assert(semi_colon == ";"), stmt
    return node("expr_stmt", [xml_of_ast_expr(expr)])

def xml_of_ast_def(top_def):
    """
    long f(long x) { return x; } ->
      <fun_def>
       <name>f</name>
       <params>
        <param>
         <type><primitive_type>long</primitive_type></type><name>x</name>
        </param>
       </params>
       <return_type><primitive_type>long</primitive_type></return_type>
       <body><return><var>x</var></return></body>
      </fun_def>
    """
    (return_type, name, lparen, params, rparen, stmt) = top_def
    assert((lparen, rparen) == ("(", ")")), top_def
    return node("fun_def", [node("name", [text(name)]),
                            node("params", [xml_of_ast_param(p) for p in del_commas(params)]),
                            node("return_type", [xml_of_ast_type(return_type)]),
                            node("body", [xml_of_ast_stmt(stmt)])])

def xml_of_ast_program(ast):
    """
    program
    """
    return node("program", [xml_of_ast_def(x) for x in ast])

def main():
    """
    main
    """
    ast = tatsu.util.generic_main(minc_grammar.main,
                                  minc_grammar.minCParser,
                                  name='minC')
    program_xml = xml_of_ast_program(ast)
    program_xml.setAttribute("xmlns", "https://program.com")
    xml_s = program_xml.toprettyxml(indent=" ")
    print(xml_s)
    return 0

main()

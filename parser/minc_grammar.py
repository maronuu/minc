#!/usr/bin/env python

# CAVEAT UTILITOR
#
# This file was automatically generated by TatSu.
#
#    https://pypi.python.org/pypi/tatsu/
#
# Any changes you make to it will be overwritten the next time
# the file is generated.

from __future__ import annotations

import sys

from tatsu.buffering import Buffer
from tatsu.parsing import Parser
from tatsu.parsing import tatsumasu
from tatsu.parsing import leftrec, nomemo, isname # noqa
from tatsu.infos import ParserConfig
from tatsu.util import re, generic_main  # noqa


KEYWORDS = {}  # type: ignore


class minCBuffer(Buffer):
    def __init__(self, text, /, config: ParserConfig = None, **settings):
        config = ParserConfig.new(
            config,
            owner=self,
            whitespace=None,
            nameguard=None,
            comments_re=None,
            eol_comments_re=None,
            ignorecase=False,
            namechars='',
            parseinfo=False,
        )
        config = config.replace(**settings)
        super().__init__(text, config=config)


class minCParser(Parser):
    def __init__(self, /, config: ParserConfig = None, **settings):
        config = ParserConfig.new(
            config,
            owner=self,
            whitespace=None,
            nameguard=None,
            comments_re=None,
            eol_comments_re=None,
            ignorecase=False,
            namechars='',
            parseinfo=False,
            keywords=KEYWORDS,
            start='start',
        )
        config = config.replace(**settings)
        super().__init__(config=config)

    @tatsumasu()
    def _start_(self):  # noqa
        self._program_()
        self._check_eof()

    @tatsumasu()
    def _program_(self):  # noqa

        def block0():
            self._definition_()
        self._closure(block0)

    @tatsumasu()
    def _definition_(self):  # noqa
        self._fun_definition_()

    @tatsumasu()
    def _fun_definition_(self):  # noqa
        self._type_expr_()
        self._identifier_()
        self._token('(')
        self._parameter_list_()
        self._token(')')
        self._compound_stmt_()

    @tatsumasu()
    def _parameter_list_(self):  # noqa

        def sep0():
            self._token(',')

        def block0():
            self._parameter_()
        self._join(block0, sep0)

    @tatsumasu()
    def _parameter_(self):  # noqa
        self._type_expr_()
        self._identifier_()

    @tatsumasu()
    def _type_expr_(self):  # noqa
        self._token('long')

    @tatsumasu()
    @nomemo
    def _stmt_(self):  # noqa
        with self._choice():
            with self._option():
                self._token(';')
            with self._option():
                self._token('continue')
                self._token(';')
            with self._option():
                self._token('break')
                self._token(';')
            with self._option():
                self._token('return')
                self._expr_()
                self._token(';')
            with self._option():
                self._compound_stmt_()
            with self._option():
                self._if_stmt_()
            with self._option():
                self._while_stmt_()
            with self._option():
                self._expr_()
                self._token(';')
            self._error(
                'expecting one of: '
                "';' 'break' 'continue' 'if' 'return'"
                "'while' '{' <compound_stmt>"
                '<equality_expr> <expr> <if_stmt>'
                '<while_stmt>'
            )

    @tatsumasu()
    def _compound_stmt_(self):  # noqa
        self._token('{')

        def block0():
            self._var_decl_()
        self._closure(block0)

        def block1():
            self._stmt_()
        self._closure(block1)
        self._token('}')

    @tatsumasu()
    def _var_decl_(self):  # noqa
        self._type_expr_()
        self._identifier_()
        self._token(';')

    @tatsumasu()
    def _if_stmt_(self):  # noqa
        self._token('if')
        self._token('(')
        self._expr_()
        self._token(')')
        self._stmt_()
        with self._optional():
            self._token('else')
            self._stmt_()

    @tatsumasu()
    def _while_stmt_(self):  # noqa
        self._token('while')
        self._token('(')
        self._expr_()
        self._token(')')
        self._stmt_()

    @tatsumasu()
    @nomemo
    def _expr_(self):  # noqa
        with self._choice():
            with self._option():
                self._equality_expr_()
                self._token('=')
                self._expr_()
            with self._option():
                self._equality_expr_()
            self._error(
                'expecting one of: '
                '<cmp_expr> <equality_expr>'
            )

    @tatsumasu()
    @leftrec
    def _equality_expr_(self):  # noqa
        with self._choice():
            with self._option():
                self._equality_expr_()
                with self._group():
                    with self._choice():
                        with self._option():
                            self._token('==')
                        with self._option():
                            self._token('!=')
                        self._error(
                            'expecting one of: '
                            "'!=' '=='"
                        )
                self._cmp_expr_()
            with self._option():
                self._cmp_expr_()
            self._error(
                'expecting one of: '
                '<additive_expr> <cmp_expr>'
                '<equality_expr>'
            )

    @tatsumasu()
    @leftrec
    def _cmp_expr_(self):  # noqa
        with self._choice():
            with self._option():
                self._cmp_expr_()
                with self._group():
                    with self._choice():
                        with self._option():
                            self._token('<=')
                        with self._option():
                            self._token('>=')
                        with self._option():
                            self._token('<')
                        with self._option():
                            self._token('>')
                        self._error(
                            'expecting one of: '
                            "'<' '<=' '>' '>='"
                        )
                self._additive_expr_()
            with self._option():
                self._additive_expr_()
            self._error(
                'expecting one of: '
                '<additive_expr> <cmp_expr>'
                '<multiplicative_expr>'
            )

    @tatsumasu()
    @leftrec
    def _additive_expr_(self):  # noqa
        with self._choice():
            with self._option():
                self._additive_expr_()
                with self._group():
                    with self._choice():
                        with self._option():
                            self._token('+')
                        with self._option():
                            self._token('-')
                        self._error(
                            'expecting one of: '
                            "'+' '-'"
                        )
                self._multiplicative_expr_()
            with self._option():
                self._multiplicative_expr_()
            self._error(
                'expecting one of: '
                '<additive_expr> <multiplicative_expr>'
                '<unary_expr>'
            )

    @tatsumasu()
    @leftrec
    def _multiplicative_expr_(self):  # noqa
        with self._choice():
            with self._option():
                self._multiplicative_expr_()
                with self._group():
                    with self._choice():
                        with self._option():
                            self._token('*')
                        with self._option():
                            self._token('/')
                        with self._option():
                            self._token('%')
                        self._error(
                            'expecting one of: '
                            "'%' '*' '/'"
                        )
                self._unary_expr_()
            with self._option():
                self._unary_expr_()
            self._error(
                'expecting one of: '
                "'!' '(' '+' '-' '~' <identifier>"
                '<multiplicative_expr> <number>'
                '<unary_expr>'
            )

    @tatsumasu()
    def _unary_expr_(self):  # noqa
        with self._choice():
            with self._option():
                self._number_()
            with self._option():
                self._identifier_()
                self._token('(')
                self._arg_list_()
                self._token(')')
            with self._option():
                self._identifier_()
            with self._option():
                self._token('(')
                self._expr_()
                self._token(')')
            with self._option():
                with self._group():
                    with self._choice():
                        with self._option():
                            self._token('+')
                        with self._option():
                            self._token('-')
                        with self._option():
                            self._token('!')
                        with self._option():
                            self._token('~')
                        self._error(
                            'expecting one of: '
                            "'!' '+' '-' '~'"
                        )
                self._unary_expr_()
            self._error(
                'expecting one of: '
                "'!' '(' '+' '-' '~' <identifier>"
                '<number> [A-Za-z_][A-Za-z_0-9]* \\d+'
            )

    @tatsumasu()
    def _arg_list_(self):  # noqa

        def sep0():
            self._token(',')

        def block0():
            self._expr_()
        self._join(block0, sep0)

    @tatsumasu()
    def _number_(self):  # noqa
        self._pattern('\\d+')

    @tatsumasu()
    def _identifier_(self):  # noqa
        self._pattern('[A-Za-z_][A-Za-z_0-9]*')


class minCSemantics:
    def start(self, ast):  # noqa
        return ast

    def program(self, ast):  # noqa
        return ast

    def definition(self, ast):  # noqa
        return ast

    def fun_definition(self, ast):  # noqa
        return ast

    def parameter_list(self, ast):  # noqa
        return ast

    def parameter(self, ast):  # noqa
        return ast

    def type_expr(self, ast):  # noqa
        return ast

    def stmt(self, ast):  # noqa
        return ast

    def compound_stmt(self, ast):  # noqa
        return ast

    def var_decl(self, ast):  # noqa
        return ast

    def if_stmt(self, ast):  # noqa
        return ast

    def while_stmt(self, ast):  # noqa
        return ast

    def expr(self, ast):  # noqa
        return ast

    def equality_expr(self, ast):  # noqa
        return ast

    def cmp_expr(self, ast):  # noqa
        return ast

    def additive_expr(self, ast):  # noqa
        return ast

    def multiplicative_expr(self, ast):  # noqa
        return ast

    def unary_expr(self, ast):  # noqa
        return ast

    def arg_list(self, ast):  # noqa
        return ast

    def number(self, ast):  # noqa
        return ast

    def identifier(self, ast):  # noqa
        return ast


def main(filename, **kwargs):
    if not filename or filename == '-':
        text = sys.stdin.read()
    else:
        with open(filename) as f:
            text = f.read()
    parser = minCParser()
    return parser.parse(
        text,
        filename=filename,
        **kwargs
    )


if __name__ == '__main__':
    import json
    from tatsu.util import asjson

    ast = generic_main(main, minCParser, name='minC')
    data = asjson(ast)
    print(json.dumps(data, indent=2))

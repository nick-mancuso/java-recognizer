/*
 [The "BSD licence"]
 Copyright (c) 2013 Terence Parr, Sam Harwell
 Copyright (c) 2017 Ivan Kochurkin (upgrade to Java 8)
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

lexer grammar JavaLexer;

channels { COMMENTS }

// Keywords and restricted identifiers
ABSTRACT:                'abstract';
ASSERT:                  'assert';
BOOLEAN:         'boolean';
BREAK:           'break';
BYTE:            'byte';
CASE:            'case';
CATCH:           'catch';
CHAR:            'char';
CLASS:           'class';
CONST:           'const';
CONTINUE:        'continue';
DEFAULT:         'default';
DO:              'do';
DOUBLE:          'double';
ELSE:            'else';
ENUM:                    'enum';
EXTENDS:          'extends';
FINAL:                   'final';
FINALLY:         'finally';
FLOAT:           'float';
FOR:             'for';
IF:              'if';
GOTO:            'goto';
IMPLEMENTS:      'implements';
IMPORT:                  'import';
INSTANCEOF:      'instanceof';
INT:             'int';
INTERFACE:       'interface';
LONG:            'long';
NATIVE:          'native';
NEW:             'new';
PACKAGE:         'package';
PRIVATE:         'private';
PROTECTED:       'protected';
PUBLIC:          'public';
RETURN:          'return';
SHORT:           'short';
STATIC:          'static';
STRICTFP:                'strictfp';
SUPER:           'super';
SWITCH:          'switch';
SYNCHRONIZED:    'synchronized';
THIS:            'this';
THROW:           'throw';
THROWS:          'throws';
TRANSIENT:       'transient';
TRY:             'try';
VOID:            'void';
VOLATILE:        'volatile';
WHILE:           'while';
RECORD:          'record';
YIELD:           'yield';
NON_SEALED:      'non-sealed';
SEALED:          'sealed';
PERMITS:         'permits';

// Literals
DECIMAL_LONG:    ('0' | [1-9] (Digits? | '_'+ Digits)) [lL];
DECIMAL_LITERAL:         ('0' | [1-9] (Digits? | '_'+ Digits));

HEX_LONG:        '0' [xX] [0-9a-fA-F] ([0-9a-fA-F_]* [0-9a-fA-F])? [lL];
HEX_LITERAL:             '0' [xX] [0-9a-fA-F] ([0-9a-fA-F_]* [0-9a-fA-F])?;

OCT_LONG:        '0' '_'* [0-7] ([0-7_]* [0-7])? [lL];
OCT_LITERAL:             '0' '_'* [0-7] ([0-7_]* [0-7])?;

BINARY_LONG:     '0' [bB] [01] ([01_]* [01])? [lL];
BINARY_LITERAL:          '0' [bB] [01] ([01_]* [01])?;


DOUBLE_LITERAL:          (Digits '.' Digits? | '.' Digits) ExponentPart? [dD]
             |           Digits (ExponentPart [dD] | [dD])
             ;
FLOAT_LITERAL:           (Digits '.' Digits? | '.' Digits) ExponentPart? [fF]?
             |           Digits (ExponentPart [fF]? | [fF])
             ;


HEX_DOUBLE_LITERAL:      '0' [xX] (HexDigits '.'? | HexDigits? '.' HexDigits)
                         [pP] [+-]? Digits [dD];

HEX_FLOAT_LITERAL:       '0' [xX] (HexDigits '.'? | HexDigits? '.' HexDigits)
                         [pP] [+-]? Digits [fFdD]?;

TRUE:            'true';

FALSE:           'false';

CHAR_LITERAL:            '\'' (EscapeSequence | ~['\\\r\n]) '\'';

STRING_LITERAL:          '"' (EscapeSequence | ~["\\\r\n])* '"';

TEXT_BLOCK_BEGIN: '"' '"' '"' -> pushMode(TextBlock);

NULL:            'null';

// Separators

LPAREN:                  '(';
RPAREN:                  ')';
LCURLY:                  '{';
RCURLY:                  '}';
LBRACK:                  '[';
RBRACK:                  ']';
SEMI:                    ';';
COMMA:                   ',';
DOT:                     '.';

// Operators

ASSIGN:                  '=';
GT:                      '>';
LT:                      '<';
LNOT:                    '!';
BNOT:                    '~';
QUESTION:                '?';
COLON:                   ':';
EQUAL:                   '==';
LE:                      '<=';
GE:                      '>=';
NOT_EQUAL:               '!=';
LAND:                    '&&';
LOR:                     '||';
INC:                     '++';
DEC:                     '--';
PLUS:                    '+';
MINUS:                   '-';
STAR:                    '*';
DIV:                     '/';
BAND:                    '&';
BOR:                     '|';
BXOR:                    '^';
MOD:                     '%';
//SR:                    '>>'; handled in parser
//SL:                    '<<'; handled in parser

PLUS_ASSIGN:             '+=';
MINUS_ASSIGN:            '-=';
STAR_ASSIGN:             '*=';
DIV_ASSIGN:              '/=';
BAND_ASSIGN:             '&=';
BOR_ASSIGN:              '|=';
BXOR_ASSIGN:             '^=';
MOD_ASSIGN:              '%=';
SL_ASSIGN:               '<<=';
SR_ASSIGN:               '>>=';
//BSR:                   '>>>'; handled in parser
BSR_ASSIGN:              '>>>=';

// Java 8 tokens

LAMBDA:                  '->';
DOUBLE_COLON:            '::';

// Additional symbols not defined in the lexical specification

AT:                      '@';
ELLIPSIS:                '...';

// Whitespace and comments
WS:[ \t\r\n\u000C]+ -> skip;

BLOCK_COMMENT_BEGIN: '/*' .*? '*/' -> channel(COMMENTS);

SINGLE_LINE_COMMENT:  '//'  ~[\r\n]* ( '\n' | '\r' ('\n')? | /** nothing */ ) -> channel(COMMENTS);

// Identifiers

IDENT:         Letter LetterOrDigit*;

// Fragment rules

fragment ExponentPart
    : [eE] [+-]? Digits
    ;

fragment EscapeSequence
    : '\\'
        ('u'+ ( '0' '0' '5' ('c' | 'C' )
                ('\\' 'u'+ HexDigit HexDigit HexDigit HexDigit | StandardEscape)
              | HexDigit HexDigit HexDigit HexDigit
              )
        | StandardEscape
        )
    ;

fragment StandardEscape
    : [btnfrs"'\\]
    | [0-3] (([0-7]) [0-7]?)?
    | [4-7] ([0-9])?
    ;

fragment HexDigits
    : HexDigit ((HexDigit | '_')* HexDigit)?
    ;

fragment HexDigit
    : [0-9a-fA-F]
    ;

fragment Digits
    : [0-9] ([0-9_]* [0-9])?
    ;

fragment LetterOrDigit
    : Letter
    | [0-9]
    ;

fragment Letter
    // these are the "java letters" below 0x7F
    : [a-zA-Z$_]
    // covers all characters above 0x7F which are not a surrogate
    | ~[\u0000-\u007F\uD800-\uDBFF]
    // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
    | [\uD800-\uDBFF] [\uDC00-\uDFFF]
    ;

// Text block lexical mode
mode TextBlock;
    TEXT_BLOCK_CONTENT
        : ( TwoDoubleQuotes
          | OneDoubleQuote
          | Newline
          | ~'"'
          | TextBlockStandardEscape
          )+
        ;

    TEXT_BLOCK_END
        : '"' '"' '"' -> popMode
        ;

    // Text block fragment rules
    fragment TextBlockStandardEscape
        :   '\\' [btnfrs"'\\]
        ;

    fragment Newline
        :  '\n' | '\r' ('\n')?
        ;

    fragment TwoDoubleQuotes
        :   '"''"' ( Newline | ~'"' )
        ;

    fragment OneDoubleQuote
        :   '"' ( Newline | ~'"' )
        ;

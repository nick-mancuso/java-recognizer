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

parser grammar JavaParser;

options { tokenVocab=JavaLexer; }

compilationUnit
    : packageDeclaration? importDeclaration* typeDeclaration* EOF
    ;

packageDeclaration
    : annotation* PACKAGE qualifiedName SEMI
    ;

importDeclaration
    : IMPORT STATIC? qualifiedName (DOT '*')? SEMI
    ;

typeDeclaration
    : classOrInterfaceModifier*
      (classDeclaration | enumDeclaration | interfaceDeclaration | annotationTypeDeclaration)
    | SEMI
    ;

modifier
    : classOrInterfaceModifier
    | NATIVE
    | SYNCHRONIZED
    | TRANSIENT
    | VOLATILE
    ;

classOrInterfaceModifier
    : annotation
    | PUBLIC
    | PROTECTED
    | PRIVATE
    | STATIC
    | ABSTRACT
    | FINAL    // FINAL for class only -- does not apply to interfaces
    | STRICTFP
    ;

variableModifier
    : FINAL
    | annotation
    ;

classDeclaration
    : CLASS id typeParameters?
      (EXTENDS typeType)?
      (IMPLEMENTS typeList)?
      (PERMITS typeList)?
      classBody
    ;

typeParameters
    : LT typeParameter (COMMA typeParameter)* GT
    ;

typeParameter
    : annotation* id (EXTENDS annotation* typeBound)?
    ;

typeBound
    : typeType (BAND typeType)*
    ;

enumDeclaration
    : ENUM id (IMPLEMENTS typeList)? LCURLY enumConstants? COMMA? enumBodyDeclarations? RCURLY
    ;

enumConstants
    : enumConstant (COMMA enumConstant)*
    ;

enumConstant
    : annotation* id arguments? classBody?
    ;

enumBodyDeclarations
    : SEMI classBodyDeclaration*
    ;

interfaceDeclaration
    : INTERFACE id typeParameters?
      (EXTENDS typeList)?
      (PERMITS typeList)?
      interfaceBody
    ;

classBody
    : LCURLY classBodyDeclaration* RCURLY
    ;

interfaceBody
    : LCURLY interfaceBodyDeclaration* RCURLY
    ;

classBodyDeclaration
    : SEMI
    | STATIC? block
    | modifier* memberDeclaration
    ;

memberDeclaration
    : methodDeclaration
    | genericMethodDeclaration
    | fieldDeclaration
    | constructorDeclaration
    | genericConstructorDeclaration
    | interfaceDeclaration
    | annotationTypeDeclaration
    | classDeclaration
    | enumDeclaration
    ;

/* We use rule this even for void methods which cannot have [] after parameters.
   This simplifies grammar and we can consider void to be a type, which
   renders the [] matching as a context-sensitive issue or a semantic check
   for invalid return type after parsing.
 */
methodDeclaration
    : typeTypeOrVoid id formalParameters (LBRACK RBRACK)*
      (THROWS qualifiedNameList)?
      methodBody
    ;

methodBody
    : block
    | SEMI
    ;

typeTypeOrVoid
    : typeType
    | VOID
    ;

genericMethodDeclaration
    : typeParameters methodDeclaration
    ;

genericConstructorDeclaration
    : typeParameters constructorDeclaration
    ;

constructorDeclaration
    : id formalParameters (THROWS qualifiedNameList)? constructorBody=block
    ;

fieldDeclaration
    : typeType variableDeclarators SEMI
    ;

interfaceBodyDeclaration
    : modifier* interfaceMemberDeclaration
    | SEMI
    ;

interfaceMemberDeclaration
    : constDeclaration
    | interfaceMethodDeclaration
    | genericInterfaceMethodDeclaration
    | interfaceDeclaration
    | annotationTypeDeclaration
    | classDeclaration
    | enumDeclaration
    ;

constDeclaration
    : typeType constantDeclarator (COMMA constantDeclarator)* SEMI
    ;

constantDeclarator
    : id (LBRACK RBRACK)* '=' variableInitializer
    ;

// Early versions of Java allows brackets after the method name, eg.
// public int[] return2DArray() [] { ... }
// is the same as
// public int[][] return2DArray() { ... }
interfaceMethodDeclaration
    : interfaceMethodModifier* (typeTypeOrVoid | typeParameters annotation* typeTypeOrVoid)
      id formalParameters (LBRACK RBRACK)* (THROWS qualifiedNameList)? methodBody
    ;

// Java8
interfaceMethodModifier
    : annotation
    | PUBLIC
    | ABSTRACT
    | DEFAULT
    | STATIC
    | STRICTFP
    ;

genericInterfaceMethodDeclaration
    : typeParameters interfaceMethodDeclaration
    ;

variableDeclarators
    : variableDeclarator (COMMA variableDeclarator)*
    ;

variableDeclarator
    : variableDeclaratorId ('=' variableInitializer)?
    ;

variableDeclaratorId
    : id (LBRACK RBRACK)*
    ;

variableInitializer
    : arrayInitializer
    | expression
    ;

arrayInitializer
    : LCURLY (variableInitializer (COMMA variableInitializer)* (COMMA)? )? RCURLY
    ;

classOrInterfaceType
    : id typeArguments? (DOT id typeArguments?)*
    ;

typeArgument
    : typeType
    | annotation* '?' ((EXTENDS | SUPER) typeType)?
    ;

qualifiedNameList
    : qualifiedName (COMMA qualifiedName)*
    ;

formalParameters
    : LPAREN formalParameterList? RPAREN
    ;

formalParameterList
    : formalParameter (COMMA formalParameter)* (COMMA lastFormalParameter)?
    | lastFormalParameter
    ;

formalParameter
    : variableModifier* typeType variableDeclaratorId
    ;

lastFormalParameter
    : variableModifier* typeType annotation* '...' variableDeclaratorId
    ;

qualifiedName
    : id (DOT id)*
    ;

literal
    : integerLiteral
    | floatLiteral
    | CHAR_LITERAL
    | STRING_LITERAL
    | BOOLEAN
    | NULL
    ;

integerLiteral
    : DECIMAL_LITERAL
    | HEX_LITERAL
    | OCT_LITERAL
    | BINARY_LITERAL
    ;

floatLiteral
    : FLOAT_LITERAL
    | HEX_FLOAT_LITERAL
    ;

// ANNOTATIONS
altAnnotationQualifiedName
    : (id DOT)* '@' id
    ;

annotation
    : ('@' qualifiedName | altAnnotationQualifiedName) (LPAREN ( elementValuePairs | elementValue )? RPAREN)?
    ;

elementValuePairs
    : elementValuePair (COMMA elementValuePair)*
    ;

elementValuePair
    : id '=' elementValue
    ;

elementValue
    : expression
    | annotation
    | elementValueArrayInitializer
    ;

elementValueArrayInitializer
    : LCURLY (elementValue (COMMA elementValue)*)? (COMMA)? RCURLY
    ;

annotationTypeDeclaration
    : '@' INTERFACE id annotationTypeBody
    ;

annotationTypeBody
    : LCURLY (annotationTypeElementDeclaration)* RCURLY
    ;

annotationTypeElementDeclaration
    : modifier* annotationTypeElementRest
    | SEMI // this is not allowed by the grammar, but apparently allowed by the actual compiler
    ;

annotationTypeElementRest
    : typeType annotationMethodOrConstantRest SEMI
    | classDeclaration SEMI?
    | interfaceDeclaration SEMI?
    | enumDeclaration SEMI?
    | annotationTypeDeclaration SEMI?
    ;

annotationMethodOrConstantRest
    : annotationMethodRest
    | annotationConstantRest
    ;

annotationMethodRest
    : id LPAREN RPAREN defaultValue?
    ;

annotationConstantRest
    : variableDeclarators
    ;

defaultValue
    : DEFAULT elementValue
    ;

// STATEMENTS / BLOCKS

block
    : LCURLY blockStatement* RCURLY
    ;

blockStatement
    : localVariableDeclaration SEMI
    | statement
    | localTypeDeclaration
    ;

localVariableDeclaration
    : variableModifier* typeType variableDeclarators
    ;

localTypeDeclaration
    : classOrInterfaceModifier*
      (classDeclaration | interfaceDeclaration)
    | SEMI
    ;

statement
    : blockLabel=block
    | ASSERT expression (COLON expression)? SEMI
    | IF parExpression statement (ELSE statement)?
    | FOR LPAREN forControl RPAREN statement
    | WHILE parExpression statement
    | DO statement WHILE parExpression SEMI
    | TRY block (catchClause+ finallyBlock? | finallyBlock)
    | TRY resourceSpecification block catchClause* finallyBlock?
    | SWITCH parExpression LCURLY switchBlockStatementGroup* switchLabel* RCURLY
    | SYNCHRONIZED parExpression block
    | RETURN expression? SEMI
    | THROW expression SEMI
    | BREAK id? SEMI
    | CONTINUE id? SEMI
    | SEMI
    | statementExpression=expression SEMI
    | identifierLabel=id COLON statement
    ;

switchExpressionOrStatement
    : SWITCH parExpression LCURLY
      {switchBlockDepth++;}
      switchBlock
      {switchBlockDepth--;}
      RCURLY
    ;

switchBlock
    : switchLabeledRule+                                                   #switchRules
    | switchBlockStatementGroup* emptyLabels+=switchLabel*         #switchBlocks
    ;

switchLabeledRule
    : switchLabeledExpression
    | switchLabeledBlock
    | switchLabeledThrow
    ;

switchLabeledExpression
    : switchLabel LAMBDA expression SEMI
    ;

switchLabeledBlock
    : switchLabel LAMBDA block
    ;

switchLabeledThrow
    : switchLabel LAMBDA THROW expression SEMI
    ;


catchClause
    : CATCH LPAREN variableModifier* catchType id RPAREN block
    ;

catchType
    : qualifiedName (BOR qualifiedName)*
    ;

finallyBlock
    : FINALLY block
    ;

resourceSpecification
    : LPAREN resources SEMI? RPAREN
    ;

resources
    : resource (SEMI resource)*
    ;

resource
    : variableModifier* classOrInterfaceType variableDeclaratorId '=' expression
    ;

/** Matches cases then statements, both of which are mandatory.
 *  To handle empty cases at the end, we add switchLabel* to statement.
 */
switchBlockStatementGroup
    : switchLabel+ blockStatement+
    ;

switchLabel
    : CASE (constantExpression=expression | enumConstantName=id) COLON
    | DEFAULT COLON
    ;

forControl
    : enhancedForControl
    | forInit? SEMI expression? SEMI forUpdate=expressionList?
    ;

forInit
    : localVariableDeclaration
    | expressionList
    ;

enhancedForControl
    : variableModifier* typeType variableDeclaratorId COLON expression
    ;

// EXPRESSIONS

parExpression
    : LPAREN expression RPAREN
    ;

expressionList
    : expression (COMMA expression)*
    ;

methodCall
    : id LPAREN expressionList? RPAREN
//    | THIS LPAREN expressionList? RPAREN
//    | SUPER LPAREN expressionList? RPAREN
    ;

expression
    : primary
    | expression bop=DOT
      ( id
      | methodCall
      | THIS
      | NEW nonWildcardTypeArguments? innerCreator
      | SUPER superSuffix
      | explicitGenericInvocation
      )
    | expression LBRACK expression RBRACK
    | methodCall
    | NEW creator
    | expression postfix=(INC | DEC)
    | prefix=(PLUS | MINUS | INC | DEC) expression
    | prefix=(BNOT | LNOT) expression
    // Java 8 methodReference
    | expression DOUBLE_COLON typeArguments? id
    | typeType DOUBLE_COLON (typeArguments? id | NEW)
    | classType DOUBLE_COLON typeArguments? NEW
    | LPAREN annotation* typeType (BAND typeType)* RPAREN expression
    | expression bop=(STAR | DIV | MOD) expression
    | expression bop=(PLUS|MINUS) expression
    | expression (LT LT | GT GT GT | GT GT) expression
    | expression bop=(LE | GE | GT | LT) expression
    | expression bop=INSTANCEOF (patternDefinition | typeType)
    | expression bop=(EQUAL | NOT_EQUAL) expression
    | expression bop=BAND expression
    | expression bop=BXOR expression
    | expression bop=BOR expression
    | expression bop=LAND expression
    | expression bop=LOR expression
    | <assoc=right> expression bop='?' expression COLON expression
    | <assoc=right> expression
      bop=(ASSIGN | PLUS_ASSIGN | MINUS_ASSIGN | STAR_ASSIGN | DIV_ASSIGN 
            | BAND_ASSIGN | BOR_ASSIGN | BXOR_ASSIGN | SR_ASSIGN 
            | BSR_ASSIGN | SL_ASSIGN | MOD_ASSIGN)
      expression
    | lambdaExpression
    ;

// Java8
lambdaExpression
    : lambdaParameters LAMBDA lambdaBody
    ;

// Java8
lambdaParameters
    : id
    | LPAREN formalParameterList? RPAREN
    | LPAREN id (COMMA id)* RPAREN
    ;

// Java8
lambdaBody
    : expression
    | block
    ;

primary
    : switchExpressionOrStatement
    | LPAREN expression RPAREN
    | THIS
    | SUPER
    | literal
    | id
    | typeTypeOrVoid DOT CLASS
    ;

classType
    : (classOrInterfaceType DOT)? annotation* id typeArguments?
    ;

creator
    : nonWildcardTypeArguments createdName classCreatorRest
    | createdName (arrayCreatorRest | classCreatorRest)
    ;

createdName
    : id typeArgumentsOrDiamond? (DOT annotation id typeArgumentsOrDiamond?)*
    | primitiveType
    ;

innerCreator
    : annotation* id nonWildcardTypeArgumentsOrDiamond? classCreatorRest
    ;

arrayCreatorRest
    : LBRACK
        (RBRACK (LBRACK RBRACK)* arrayInitializer 
        | expression RBRACK (LBRACK expression RBRACK)* arrayDeclarator*
        )
    ;

classCreatorRest
    : arguments classBody?
    ;

explicitGenericInvocation
    : nonWildcardTypeArguments explicitGenericInvocationSuffix
    ;

typeArgumentsOrDiamond
    : LT GT
    | typeArguments
    ;

nonWildcardTypeArgumentsOrDiamond
    : LT GT
    | nonWildcardTypeArguments
    ;

nonWildcardTypeArguments
    : LT typeList GT
    ;

typeList
    : typeType (COMMA typeType)*
    ;

typeType
    : annotation* (classOrInterfaceType | primitiveType) arrayDeclarator*
    ;
    
arrayDeclarator
    : annotation* LBRACK RBRACK
    ;

primitiveType
    : BOOLEAN
    | CHAR
    | BYTE
    | SHORT
    | INT
    | LONG
    | FLOAT
    | DOUBLE
    ;

typeArguments
    : LT typeArgument (COMMA typeArgument)* GT
    ;

superSuffix
    : arguments
    | DOT id arguments?
    ;

explicitGenericInvocationSuffix
    : SUPER superSuffix
    | id arguments
    ;

arguments
    : LPAREN expressionList? RPAREN
    ;
    
patternDefinition
    : patternVariableDefinition
    ;
    
patternVariableDefinition
    : variableModifier typeType id
    ;

// Handle the 'keyword as identifier' problem
id  : RECORD
    | YIELD
    | NON_SEALED
    | SEALED
    | PERMITS
    | IDENT
    ;

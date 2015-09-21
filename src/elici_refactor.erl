%%%-------------------------------------------------------------------
%%% @author Rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc
%%%
%%% @end
%%% Created : 15. 八月 2015 16:27
%%%-------------------------------------------------------------------
-module(elici_refactor).
-author("Rongjie").

-include("elici.hrl").

%% API
-export([refactor_forms/1, expend_if_forms/1, refactor_wait_forms/1]).

%%语法树重组
refactor_forms(SyntaxForms) ->
	ExpendedForms = expend_if_forms(SyntaxForms),
	refactor_wait_forms(ExpendedForms).


%% @doc 重组Wait表达式
refactor_wait_forms(SyntaxForms) ->
	wait_grammar(SyntaxForms).

wait_grammar(SyntaxForms) ->
	case SyntaxForms of
		#grammar{statements = Statements} = Grammar ->
			NewStatements = wait_statements(Statements, []),
			Grammar#grammar{statements = NewStatements};
		_ ->
			SyntaxForms
	end.

wait_statements([], RefactorForms) ->
	lists:reverse(RefactorForms);
wait_statements([#wait_statement{} = Statement| Statements], RefactorForms) ->
	NewStatement = Statement#wait_statement{statements = wait_statements(Statements, [])},
	lists:reverse([NewStatement | RefactorForms]);
wait_statements([#if_statement{if_clause = IfClauses, else_clause = ElseStatements} = IfStatement], RefactorForms) ->
	lists:reverse([IfStatement#if_statement{if_clause = wait_if_clauses(IfClauses),
	                                        else_clause = wait_else_clause(ElseStatements )} | RefactorForms]);
wait_statements([Statement | Statements], RefactorForms) ->
	wait_statements(Statements, [Statement | RefactorForms]).

wait_if_clauses(#if_clause{statements  = Statements} = IfClause) ->
	IfClause#if_clause{statements = wait_statements(Statements, [])}.

wait_else_clause(#if_statement{} = IfStatement) ->
	wait_statements([IfStatement], []);
wait_else_clause(ElseClause) ->
	wait_statements(ElseClause, []).

%% @doc 展开if表达式
expend_if_forms(SyntaxForms) ->
	expend_grammar(SyntaxForms).
expend_grammar(SyntaxForms) ->
	case SyntaxForms of
		#grammar{statements = Statements} = Grammar ->
			NewStatements = expend_statements(Statements, []),
			Grammar#grammar{statements = NewStatements};
		_ ->
			SyntaxForms
	end.

expend_statements([], Expended) ->
	lists:reverse(Expended);
expend_statements([#if_statement{} = IfStatement | Statements], Expended) ->
	lists:reverse([build_if_statement(IfStatement, expend_statements(Statements, []))| Expended]);
expend_statements([Statement | Statements], RefactorForms) ->
	expend_statements(Statements, [Statement | RefactorForms]).

build_if_statement(#if_statement{if_clause = #if_clause{statements = S1}  = IfClause, else_clause = ElseClause} = IfStatement,
                   Statements) ->
	NewIfClause = IfClause#if_clause{statements = expend_statements(lists:append(S1,Statements), [])},
	IfStatement#if_statement{if_clause = NewIfClause, else_clause = build_if_statement(ElseClause, Statements)};
build_if_statement(ElseStatements, Statements) ->
	lists:append(ElseStatements, Statements).

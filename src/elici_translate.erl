%%%-------------------------------------------------------------------
%%% @author rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc
%%%      语言转换,与elici_translate_erlang的区别是,该文件处理的是elici语言,
%%%  而elici_translate_erlang是为了项目实现的一个脚本生成器
%%%
%%% @end
%%% Created : 28. 七月 2015 21:31
%%%-------------------------------------------------------------------
-module(elici_translate).
-author("rongjie").
-include("elici_log.hrl").
-include("elici.hrl").
%% API
-export([forms/1, forms/2]).
-compile(export_all).
-callback begin_translate(SyntaxFroms :: list(), RecElici :: #elici{}) -> NewRecElici :: #elici{}.
-callback end_translate(RecElici :: #elici{}) -> NewRecElici :: #elici{}.

forms(SyntaxForms) ->
	forms(SyntaxForms, #{}).

forms(SyntaxForms, RecElici) ->
	forms(elici_translate_erlang, SyntaxForms, RecElici).
forms(WorkModule, SyntaxForms, RecElici) ->
	{ok, NewSyntaxForms, NewRecElici} = WorkModule:begin_translate(SyntaxForms, RecElici),
	%%io:format("Handle SyntaxForms:~p~n", [NewSyntaxForms]),
	NewRecEliciEnd = grammar(NewSyntaxForms, NewRecElici),
	WorkModule:end_translate(NewRecEliciEnd).

%% 语法分析
grammar(#grammar{type = script, meta = Meta, statements = Statements} = Grammar, RecElici) ->
	NewRecElici = handle_forms(Grammar, RecElici),
	NewRecElici1 = meta(Meta, NewRecElici),
	statements(Statements, NewRecElici1);
grammar(Grammar, _RecElici) ->
	?THROW_UNEXPECTED(unexpected_grammar, Grammar).
meta([], RecElici) ->
	RecElici;
meta(#meta{param = Args} = Meta, RecElici) ->
	NewRecElici = args(Args, RecElici#elici{content = []}),
	handle_forms(Meta#meta{param = NewRecElici}, RecElici).


statements([], RecElici) ->
	RecElici;
statements([#if_statement{} = IfStatement|OtherStatements], RecElici) ->
	NewRecElici = if_statement(IfStatement, RecElici),
	statements(OtherStatements, NewRecElici);

statements([#wait_statement{} = WaitStatement | OtherStatement], RecElici) ->
	NewRecElici = wait_statement(WaitStatement, RecElici),
	statements(OtherStatement, NewRecElici);

statements([#return_statement{} = ReturnStatement | OtherStatements], RecElici) ->
	NewRecElici = return_statement(ReturnStatement, RecElici),
	statements(OtherStatements, NewRecElici);

statements([Statement| OtherStatement],  #elici{stmt_delim = Delim} = RecElici) ->
	NewRecElici = statement(Statement, RecElici),
	%%决定是否需要分隔
	NewContent =
		if (Delim == ";") ->
			Str = Delim ++  "\n",
			[Str | NewRecElici#elici.content];
			true ->
				case OtherStatement of
					[] -> NewRecElici#elici.content;
					_ ->
						Str = Delim ++  "\n",
						[Str | NewRecElici#elici.content]
				end
		end,
	statements(OtherStatement, NewRecElici#elici{content = NewContent});

statements(Statements, _RecElici) ->
	?THROW_UNEXPECTED(unexpected_statements, Statements).

if_statement([], RecElici) ->
	RecElici;
if_statement(#if_statement{if_clause = IfClause, else_clause = ElseStatements} = IfStat,
             RecElici) ->
	{CondRecElici, TrueRecElici} = if_clause(IfClause, RecElici),
    FalseRecElici = if_statement(ElseStatements, RecElici#elici{content = []}),
    handle_forms(IfStat#if_statement{type = [CondRecElici, TrueRecElici, FalseRecElici]}, RecElici#elici{bindings = FalseRecElici#elici.bindings});

if_statement(ElseStatements, RecElici) ->
	statements(ElseStatements, RecElici).
	%%?THROW_UNEXPECTED(unexpected_if_statement, IfStatement).

%%如果是erlang代码,由if的递归定义对应Erlang代码为:
%%case Conditions of
%%  true ->
%%      Statements;
if_clause(#if_clause{conditions = Expresses, statements = Statements},
           #elici{indent = Indent} = RecElici) ->
	CondRecElici = expresses(Expresses,  RecElici#elici{indent =  0, content = []}),
	%%io:format("~nif clause cond:~p, =>Content:~p", [Expresses, CondRecElici#elici.content]),
	TrueRecElici = statements(Statements, CondRecElici#elici{indent = Indent + ?INDENT_SPACE *2 , content = []}),
	{CondRecElici, TrueRecElici};

if_clause(Other, _RecElici) ->
    ?THROW_UNEXPECTED(expected_if_clause, Other).

wait_statement(#wait_statement{args =  Args, statements = Statements} = WaitStatement,
    #elici{indent = Indent, waitid = WaitID} = RecElici) ->
	FunRecElici =  statements(Statements, RecElici#elici{indent = Indent + ?INDENT_SPACE, waitid = WaitID + 1, content = []}), %%生成内容
	ArgsRecElici = args(Args, RecElici#elici{content = []}),
	handle_forms(WaitStatement#wait_statement{type = [ArgsRecElici, FunRecElici]},
	             RecElici);
wait_statement(WaitStatement, _RecElici) ->
	?THROW_UNEXPECTED(unexpected_wait_statement, WaitStatement).

return_statement(ReturnStatement, RecElici) ->
	#return_statement{clause = Statement} = ReturnStatement,
	NewRecElici = statement(Statement, RecElici#elici{content = []}),
	handle_forms(ReturnStatement#return_statement{type = NewRecElici}, RecElici).

statement(#statement{type = expresses, clause = Expresses}, RecElici) ->
	expresses(Expresses, RecElici);
statement(Statement, _RecElici) ->
	?THROW_UNEXPECTED(unexpected_statement, Statement).

expresses(#expresses{type = bool_expr, exprs = BoolExprs}, RecElici) ->
	bool_expr(BoolExprs, RecElici);
expresses(#expresses{type = assign_expr, exprs = AssignExprs}, RecElici) ->
	assign_expr(AssignExprs, RecElici);

expresses(Expresses, _RecElici) ->
	?THROW_UNEXPECTED(unexpected_expresses, Expresses).

assign_expr(#assign_expr{expresses = Expresses} = Forms, #elici{indent = Indent} = RecElici) ->
	NewRecElici1 = expresses(Expresses, RecElici#elici{content = [], indent =  0}),
	NewRecElici2 = handle_forms(Forms#assign_expr{expresses = NewRecElici1}, RecElici),
	NewRecElici2#elici{indent = Indent};

assign_expr(AssignExprs, _RecElici) ->
	?THROW_UNEXPECTED(expected_assign_expr, AssignExprs).

bool_expr(#bool_expr{left = LeftBoolExpr, right = CompExpr} = BoolExpr, RecElici) ->
	NewRecElici = bool_expr(LeftBoolExpr, RecElici),
	NewRecElici2 = handle_forms(BoolExpr, NewRecElici),
    comp_expr(CompExpr, NewRecElici2);
bool_expr(CompExpr, RecElici) ->
	comp_expr(CompExpr, RecElici).


comp_expr(#comp_expr{left = LeftMathExpr, right = RightMathExpr} = CompExpr,RecElici) ->
	RecElici1 = math_expr(LeftMathExpr, RecElici),
	RecElici2 = handle_forms(CompExpr, RecElici1),
	math_expr(RightMathExpr, RecElici2);

comp_expr(MathExpr,RecElici) ->
	math_expr(MathExpr, RecElici).

math_expr(#math_expr{left = SubMathExpr, right = MultExpr} = MathExpr, RecElici) ->
	RecElici1 = math_expr(SubMathExpr, RecElici),
	RecElici2 = handle_forms(MathExpr, RecElici1),
	mult_expr(MultExpr, RecElici2);
math_expr(MultExpr,RecElici) ->
	mult_expr(MultExpr, RecElici).

mult_expr(#mult_expr{left = LeftMultExpr, right = UnaryExpr} = MultExpr,RecElici )->
	RecElici1 = mult_expr(LeftMultExpr, RecElici),
	RecElici2 = handle_forms(MultExpr, RecElici1),
	unary_expr(UnaryExpr, RecElici2);
mult_expr(UnaryExpr, RecElici) ->
	unary_expr(UnaryExpr, RecElici).


unary_expr(#unary_expr{exprs = _UnaryExpr1} = UnaryExpr, RecElici) ->
	NewRecElici = handle_forms(UnaryExpr, RecElici),
	unary_expr(UnaryExpr, NewRecElici);

unary_expr(PowerExpr, RecElici) ->
	pow_expr(PowerExpr, RecElici).

pow_expr(#pow_expr{left = Exp, right = PowerExp} = PowExpr, RecElici) ->
	RecElici1 = expr(Exp, RecElici),
	RecElici2 = handle_forms(PowExpr, RecElici1),
	pow_expr(PowerExp, RecElici2);

pow_expr(Expr, RecElici) ->
	expr(Expr, RecElici).

expr(#expr{type = priority,clause = MathExpr} = Expr, RecElici) ->
	RecElici1 = handle_forms(Expr#expr{type = "("}, RecElici),
	RecElici2 = math_expr(MathExpr, RecElici1),
	handle_forms(Expr#expr{type = ")"}, RecElici2);

expr(#expr{type = call, clause = CallExpr}, RecElici) ->
	%%io:format("~nCall Expr:~p", [CallExpr]),
	call_expr(CallExpr, RecElici);

expr(#expr{type = list, clause = List} = Expr, RecElici) ->
	%%io:format("~n expr list:~p", [List]),
	NewRecElici = list(List, RecElici#elici{content = []}),
	handle_forms(Expr#expr{clause = NewRecElici}, RecElici);


expr(Expr, RecElici) ->
	handle_forms(Expr, RecElici).

call_expr(#call_expr{args = Args} = CallExpr, #elici{indent = Indent} = RecElici) ->
	ArgRecElici = args(Args, RecElici#elici{indent = 0, content = []}),
	NewRecElici3 = handle_forms(CallExpr#call_expr{type = ArgRecElici}, RecElici),
	NewRecElici3#elici{indent = Indent};

call_expr(Expr, _RecElici) ->
	throw({unexpected_call_expr, Expr, erlang:get_stacktrace()}).

args([], RecElici) ->
	RecElici;
args([#args{type = expresses, clause = Expresses} = Arg| Args], RecElici) ->
	NewRecElici = expresses(Expresses, RecElici),
	NewRecElici2 = handle_forms(Arg#args{other = Args}, NewRecElici),
	args(Args, NewRecElici2);
%%args([#args{type = list, clause = List} | Args] , RecElici) ->
%%	NewRecElici = list(List, RecElici),
%%	args(Args, NewRecElici);
args(Args, _RecElici) ->
	?THROW_UNEXPECTED(unexpected_args, Args).

list(Elements,  RecElici) ->
	NewRecElici = elements(Elements, RecElici#elici{content = []}),
    handle_forms(#list{elements = NewRecElici}, RecElici#elici{bindings = NewRecElici#elici.bindings}).

elements([], RecElici) ->
	RecElici;
elements([Expresses|OtherElements], RecElici) ->
	%%io:format("~nElem, Expr:~p", [Expresses]),
	NewRecElici = expresses(Expresses, RecElici),
	NewRecElici2 = handle_forms(#elements{cur_element = Expresses, other_elements = OtherElements} , NewRecElici),
	elements(OtherElements, NewRecElici2).

handle_forms(Forms, #elici{work_module = WorkModule} = RecElici) ->
	%%io:format("~nhandle forms:~p, Module:~p~n", [Forms, WorkModule]),
	Fun = element(1, Forms),
	WorkModule:Fun(Forms, RecElici).

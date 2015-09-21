%%%-------------------------------------------------------------------
%%% @author  rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc
%%%
%%% @end
%%% Created : 29. 七月 2015 19:38
%%%-------------------------------------------------------------------
-module(elici_translate_erlang).
-author("rongjie").
-include("elici_log.hrl").
-include("elici.hrl").

-behaviour(elici_translate).

%% API
%% elici_translate callback
-export([begin_translate/2, end_translate/1]).
-compile(export_all).

%% 开始处理脚本
begin_translate(SyntaxForms, RecElici) ->
	%%io:format("~nrefactor forms:~p", [SyntaxForms]),
	NewSyntaxForms = elici_refactor:refactor_forms(SyntaxForms),
	%%io:format("~n Handle Forms:~p", [NewSyntaxForms]),
	{ok, NewSyntaxForms, RecElici#elici{indent = 0}}.

end_translate(RecElici) ->
	#elici{indent =  Indent, template = OutTemplate, content = ErlangContent} = RecElici,
    EndStr = elici_util:concat(Indent, [";\n"]),
	Output = elici_util:concat_r([EndStr | ErlangContent]),
	NewOuttemplate = re:replace(OutTemplate,?ERLANG_BODY_PLACEHOLDER, string:concat(Output, ?ERLANG_BODY_PLACEHOLDER)),
	RecElici#elici{indent = 0, content = NewOuttemplate}.

grammar(#grammar{}, RecElici) ->
	RecElici.

meta(#meta{param = ParamElici}, #elici{filename = FileName, indent = Indent, scriptid = ScriptID, content = Content} = RecElici) ->
	%% 获得注释
	CommmentStr =  elici_util:concat(["\n%%", elici_util:comment(FileName), "\n"]),
	ParamStr = elici_util:concat_r(ParamElici#elici.content),
	ScriptFun = elici_util:concat(Indent, ["script_execute(", ScriptID, ", 0, [", ParamStr, "]) ->\n"]),
	RecElici#elici{content = [ScriptFun , CommmentStr| Content], indent = ?INDENT_SPACE}.

wait_statement(#wait_statement{mod = Mod, type = [ArgsRecElici, FunRecElici]},
               #elici{scriptid = ScriptID, indent = Indent, waitid = WaitID, content = Content} = RecElici) ->

	%%WaitTemplate = "%%MOD%%:wait(%%ARG%%, %%CALLBACK%%)",
	case erlang:function_exported(Mod, wait, 3) of
		true -> ok;
		_-> exit(?ERROR({wait_fun_no_define, Mod, wait}))
	end,

	ClosureStatementStr = case FunRecElici#elici.content of
		                      [] -> elici_util:concat(Indent + ?INDENT_SPACE, ["none"]);
		                      _ -> elici_util:concat_r(FunRecElici#elici.content)
	                      end,

	ClosureFunIdStr = integer_to_list(WaitID),
	ClosureFunDefineStr = elici_util:concat(Indent, ["Fun", ClosureFunIdStr, " = fun() -> \n", ClosureStatementStr, "\n"]),
	ClosureEndStr = elici_util:concat(Indent, ["end,\n"]),

	ClosureFunStr = elici_util:concat([ClosureFunDefineStr, ClosureEndStr]),
	%%io:format("~nClosureFunStr:~p", [ClosureFunStr]),

	WaitArgStr = elici_util:concat_r(ArgsRecElici#elici.content),
	%%io:format("~nWaitArgStr:~p~n", [[atom_to_list(Mod), ":wait(", WaitArgStr, ",", ScriptID, ", Fun", ClosureFunIdStr,  ")"]]),
	WaitStr = elici_util:concat(Indent, [atom_to_list(Mod), ":wait(", WaitArgStr, ",", ScriptID, ", Fun", ClosureFunIdStr,  ")"]),
	RecElici#elici{content = [WaitStr, ClosureFunStr | Content], waitid = WaitID + 1}.

return_statement(#return_statement{type = ReturnElici}, #elici{indent = Indent, content = Content} = RecElici) ->

	Str = elici_util:concat(Indent, ReturnElici#elici.content),

	RecElici#elici{content = [Str | Content]}.

if_statement(#if_statement{type = [CondRecElici, TrueRecElici, FalseRecElici], else_clause = ElseClause}, #elici{indent = Indent, content = Content} = RecElici) ->
	#elici{content = CondContent} = CondRecElici,
	#elici{content = TrueContent} = TrueRecElici,
	#elici{content = FalseContent} = FalseRecElici,

	%%CaseTmp = "case %%COND%% of",

	CondStr = elici_util:concat_r(CondContent),
	TrueStr = elici_util:concat_r(TrueContent),
	%%io:format("~nCond::~p", [CondStr]),
	FalseStr =
	case  ElseClause of
		[] -> elici_util:concat(Indent + ?INDENT_SPACE * 2, ["none\n"]);
		_ ->
			elici_util:concat_r(["\n" | FalseContent])
	end,
	CaseCond= elici_util:concat(Indent, ["case ", CondStr, " of \n"]),
	TrueSymbol = elici_util:concat(Indent + ?INDENT_SPACE, ["true -> \n"]),
	TrueOutput = elici_util:concat([TrueStr, ";\n"]),
	ElseSymbol = elici_util:concat(Indent + ?INDENT_SPACE, ["_ -> \n"]),
	ElseOutput = elici_util:concat(Indent + ?INDENT_SPACE, [FalseStr]),
	EndSymbol = elici_util:concat(Indent, ["end"]),
	IfOutput = elici_util:concat([CaseCond, TrueSymbol, TrueOutput, ElseSymbol,ElseOutput, EndSymbol]),

    RecElici#elici{content =  [IfOutput | Content]};

if_statement(#if_statement{type = else_end}, #elici{indent = Indent, content = Content} = RecElici) ->
	ElseEndStr = elici_util:concat(Indent, ["end"]),
	RecElici#elici{content = [ElseEndStr | Content]}.
%%Delete follow:
if_clause(#if_clause{type = condition_begin}, #elici{indent = Indent, content = Content} = RecElici) ->
    IfStr = elici_util:concat(Indent,  ["case "]),
    RecElici#elici{content = [IfStr | Content]};
if_clause(#if_clause{type = condition_end}, #elici{indent = Indent, content = Content} = RecElici) ->
    OfStr = elici_util:concat([" of \n"]),
    TrueStr = elici_util:concat(Indent + ?INDENT_SPACE, ["true -> \n"]),
    RecElici#elici{content = [TrueStr, OfStr | Content], indent = Indent + ?INDENT_SPACE};
if_clause(#if_clause{type = else_end}, #elici{content = Content} = RecElici) ->
	EndStr = elici_util:concat([";\n"]),
	RecElici#elici{content = [EndStr | Content]}.

assign_expr(#assign_expr{var_list = VarList,type = AssignSymbol, expresses = ExprElici}, #elici{indent = Indent, bindings = BindingMaps, content = Content} = RecElici) ->
	ExprStr = elici_util:concat_r(ExprElici#elici.content),
	case VarList of
		VarList when is_list(VarList) ->
			{ok, RebindVarList, NewBindingMaps} = elici_binds:parallel_assign(VarList, BindingMaps),
			RebindVarStr = string:join(RebindVarList, ","),
			%%io:format("~nassign rebind var:~p, expr:~p~n", [RebindVarStr, ExprStr]),
			Str = elici_util:concat(Indent, ["[", [RebindVarStr], "]", atom_to_list(AssignSymbol), ExprStr]),
			RecElici#elici{content = [Str|Content], bindings = NewBindingMaps};
		_ ->
			{ok, ReBindingVar, NewBindingMaps} = elici_binds:rebind_var(VarList, BindingMaps),
			%%io:format("~nAssign rebind var:~p, expr:~p~n", [ReBindingVar, ExprStr]),
			Str = elici_util:concat(Indent, [ReBindingVar, atom_to_list(AssignSymbol), ExprStr]),
			RecElici#elici{content = [Str|Content], bindings = NewBindingMaps}
	end.

bool_expr(#bool_expr{type = BoolType}, #elici{content = Content} = RecElici) ->
	BoolStr = case BoolType of
				  '&&'  -> " andalso ";
				  '||' -> " orelse ";
				  _ -> atom_to_list(BoolType)
	          end,
	RecElici#elici{content = [BoolStr| Content]}.

comp_expr(#comp_expr{type = CompType}, #elici{content = Content} = RecElici) ->
	RecElici#elici{content = [atom_to_list(CompType) | Content]}.

math_expr(#math_expr{type = MathType}, #elici{content = Content} = RecElici) ->

	RecElici#elici{content = [atom_to_list(MathType) | Content]}.

mult_expr(#mult_expr{type = MultType}, #elici{content = Content} = RecElici) ->
	RecElici#elici{content = [atom_to_list(MultType) | Content]}.


unary_expr(#unary_expr{type = UnaryType}, #elici{content = Content} = RecElici) ->
	RecElici#elici{content = [atom_to_list(UnaryType) | Content]}.

pow_expr(#pow_expr{type = PowerType}, #elici{content = Content} = RecElici) ->
	RecElici#elici{content = [atom_to_list(PowerType) | Content]}.


expr(#expr{type = Type, clause = Clause},#elici{scriptid = ScriptID, content = Content, bindings = BindingMaps} = RecElici) ->
	ExprStr =
	case Type of
		var ->
			ReBindingVar = elici_binds:eval_binds(Clause, BindingMaps),
			%%io:format("expr rebind var :~p, BindMaps:~p", [ReBindingVar, BindingMaps]),
			ReBindingVar;
		integer ->
			integer_to_list(Clause);
		float ->
			io_lib:format("~w", [Clause]);
		string ->
			Clause;
		atom ->
			atom_to_list(Clause);
		this ->
			ScriptID;
		list ->
			ListStr = elici_util:concat(Clause#elici.content),
			%%io:format("~nexpr list:~p", [ListStr]),
			ListStr;
		Other ->
			%%io:format("Other:~p", [Other]),
			Other
	end,
	RecElici#elici{content = [ExprStr | Content]}.


call_expr(#call_expr{type = ArgsEilci, module = Module, function = FunName},
          #elici{indent = Indent, content = Content} = RecElici) ->

	FunTemplate = "%%MODULE%%:%%FUN%%(%%ARGS%%)",
	LocalTemplate = "%%FUN%%(%%ARGS%%)",
	ArgsStr = elici_util:concat_r(ArgsEilci#elici.content),
	Argc =
	case ArgsStr of
		[] -> 0;
		_ -> erlang:length([A|| A <- ArgsEilci#elici.content, A =:= ","]) + 1
	end,

	%%io:format("~nCall expr args:~p, argc:~p", [ArgsStr, Argc]),
	Arity = Argc,

	CallStr =
	case Module of
		undefined ->
			%% 本地函数
			case elici_util:function_exist(FunName, "") of
				true ->
					Str = elici_util:re_replace(LocalTemplate, ["%%FUN%%", "%%ARGS%%"], [atom_to_list(FunName), ArgsStr]),
					elici_util:concat(Indent, [Str]);
				_ -> throw(?ERROR({local_func_no_exist, FunName}))
			end;
		_ ->
			%% 远程函数

			case erlang:is_builtin(Module, FunName, Arity) orelse erlang:function_exported(Module, FunName, Arity) of
				true ->
					Str = elici_util:re_replace(FunTemplate, ["%%MODULE%%", "%%FUN%%", "%%ARGS%%"],
					                            [atom_to_list(Module), atom_to_list(FunName), ArgsStr]),
					elici_util:concat(Indent, [Str]);
				_ -> throw(?ERROR({undefined, Module, FunName, Arity}))
			end
	end,
    %%io:format("~ncall indent===>~p:~s", [Indent, CallStr]),
	RecElici#elici{content = [CallStr | Content], indent = 0 }.
args(#args{other = Other}, #elici{content = Content} = RecElici) ->
	case Other of
		[] -> RecElici;
		_ -> RecElici#elici{content = ["," | Content]}
	end.
list(#list{elements =  #elici{content = ListContent}}, #elici{content = Content} = RecElici) ->
	ElementStr = elici_util:concat_r(ListContent),
	%%io:format("~n~p~n", [ElementStr]),
	ListStr = elici_util:concat(["[", ElementStr, "]"]),
	RecElici#elici{content = [ListStr | Content]}.

elements(#elements{other_elements = OtherElements}, #elici{content = Content} = RecElici) ->
	case OtherElements of
		[] -> RecElici;
		_ -> RecElici#elici{content = ["," | Content]}
	end.


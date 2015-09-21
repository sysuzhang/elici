%%%-------------------------------------------------------------------
%%% @author rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc
%%%
%%% @end
%%% Created : 29. 七月 2015 20:20
%%%-------------------------------------------------------------------
-module(elici_translate_csharp).
-author("rongjie").
-include("elici.hrl").

-behaviour(elici_translate).

%% elici_translate callback
-export([begin_translate/2, end_translate/1]).
-compile(export_all).

-define(ANDAND, " %%ANDAND%% ").

begin_translate(SyntaxForms, RecElici) ->
	%%io:format("Before Refactor :~p", [SyntaxForms]),
	NewSyntaxForms = elici_refactor:refactor_forms(SyntaxForms),
	%%io:format("After Refactor :~p", [NewSyntaxForms]),
	{ok, NewSyntaxForms, RecElici#elici{indent = ?INDENT_SPACE, stmt_delim = ";"}}.

end_translate(RecElici) ->
	#elici{indent =  Indent, template = OutTemplate, content = Content,stmt_type = Type} = RecElici,
	NewIndent = Indent - ?INDENT_SPACE,
	EndStr1 = elici_util:concat(Indent, ["return new object[]{};\n"]), %%函数结构
	EndStr2 = elici_util:concat(NewIndent, ["}\n\n"]),
	Output = elici_util:concat_r([EndStr2, EndStr1 | Content]),
	NewOuttemplate = re:replace(OutTemplate,?CSHARP_BODY_PLACEHOLDER, string:concat(Output, ?CSHARP_BODY_PLACEHOLDER)),
	%%统一处理&&特殊符号
	NewOuttemplate2 = re:replace(NewOuttemplate, ?ANDAND, " \\&\\& "),
	RecElici#elici{content = NewOuttemplate2, indent = 0}.


grammar(#grammar{}, RecElici) ->
	RecElici.

meta(#meta{param = ParamElici}, #elici{filename = FileName, indent = Indent, scriptid = ScriptID, content = Content} = RecElici) ->
	%% 获得注释
	CommmentStr =  elici_util:concat(Indent, ["//", elici_util:comment(FileName), "\n"]),

	ParamStr = elici_util:concat_r(ParamElici#elici.content),
	Tokens = string:tokens(ParamStr, ","),
	NewParamStr = string:join(["object " ++ Token ||Token <-Tokens], ","),

	ScriptFun = elici_util:concat(Indent, ["public object[] Script_", ScriptID, "(", NewParamStr, ")\n"]),
	LeftBrace = elici_util:concat(Indent, ["{\n"]),
	RecElici#elici{content = [LeftBrace, ScriptFun , CommmentStr| Content], indent = Indent + ?INDENT_SPACE}.

wait_statement(#wait_statement{type = [ArgsRecElici, FunRecElici]},
               #elici{indent = Indent, waitid = WaitID, content = Content} = RecElici) ->

	ClosureStatementStr = case FunRecElici#elici.content of
		                      [] -> elici_util:concat(Indent + ?INDENT_SPACE, ["null"]);
		                      _ -> elici_util:concat_r(FunRecElici#elici.content)
	                      end,

	WaitStrTemplate1 = elici_util:concat(Indent, ["var Fun%%FUNID%% = new TimerCallback((state%%STATEID%%) => {\n%%CLOSURE%% "]),
	WaitStrTemplate2 = elici_util:concat(Indent, ["var Fun%%FUNID%% = new TimerCallback((state%%STATEID%%) => {\n%%CLOSURE%% "]),
	ClosureFunIdStr = integer_to_list(WaitID),
	ClosureStr = case FunRecElici#elici.stmt_type of
					if_stmt ->
						elici_util:re_replace(WaitStrTemplate2, ["%%FUNID%%", "%%STATEID%%", "%%CLOSURE%%"], [ClosureFunIdStr, ClosureFunIdStr, ClosureStatementStr]);
		             _ ->
			             elici_util:re_replace(WaitStrTemplate1, ["%%FUNID%%", "%%STATEID%%", "%%CLOSURE%%"], [ClosureFunIdStr, ClosureFunIdStr, ClosureStatementStr])
	             end,
	EndWaitFun = elici_util:concat(Indent, ["});\n"]),
	WaitArgStr = elici_util:concat_r(ArgsRecElici#elici.content),
	WaitStr = elici_util:concat(Indent, ["ServerScriptCommon.Instance.wait(", WaitArgStr, ", Fun", ClosureFunIdStr,  ");\n"]),
	RecElici#elici{stmt_type = wait_stmt, content = [WaitStr, EndWaitFun, ClosureStr | Content], waitid = WaitID + 1}.

return_statement(#return_statement{type = ReturnElici}, #elici{indent = Indent, content = Content} = RecElici) ->
	ReturnTmp = "return new object []{%%STMT%%};\n",
	Stmt = elici_util:concat(ReturnElici#elici.content),
	ReturnStr = elici_util:re_replace(ReturnTmp, ["%%STMT%%"], [Stmt]),
	Str = elici_util:concat(Indent + ?INDENT_SPACE, [ReturnStr]),
	RecElici#elici{content = [Str | Content]}.

if_statement(#if_statement{type = [CondRecElici, TrueRecElici, FalseRecElici], else_clause = ElseClause}, #elici{indent = Indent, content = Content} = RecElici) ->
	#elici{content = CondContent} = CondRecElici,
	#elici{content = TrueContent} = TrueRecElici,
	#elici{content = FalseContent} = FalseRecElici,
	Template1 = "if(%%COND%%){\n %%TRUE%% }",
	Template2 = lists:concat(["if(%%COND%%){\n %%TRUE%%", elici_util:concat(Indent, ["} else {\n"]), "%%ELSE%%", elici_util:concat(Indent, ["}\n"])]),
	Template3 = lists:concat(["if(%%COND%%){\n %%TRUE%%", elici_util:concat(Indent, ["}else "]), "%%ELSEIF%%"]),
	%%io:format("~nElse:~p~n", [ElseClause]),
	CondStr = elici_util:concat_r(CondContent),
	TrueStr = elici_util:concat_r(TrueContent),
	ElseStr = elici_util:concat_r(FalseContent),

	%%io:format("~nCondStr:~p", [CondStr]),

	OutputStr =
	case ElseClause of
		[] ->
			%%io:format("~nnull replace, tmp:~p", [Template1]),
			elici_util:re_replace(Template1, ["%%COND%%", "%%TRUE%%"], [CondStr, TrueStr]);
		[#if_statement{}] ->
			elici_util:re_replace(Template3, ["%%COND%%", "%%TRUE%%", "%%ELSEIF%%"], [CondStr, TrueStr, ElseStr]);
		_ ->
			elici_util:re_replace(Template2, ["%%COND%%", "%%TRUE%%", "%%ELSE%%" ], [CondStr, TrueStr, ElseStr])
	end,

	%%io:format("~nOutputStr:~p", [OutputStr]),

	RecElici#elici{stmt_type = if_stmt, content = [elici_util:concat(Indent, [OutputStr]) | Content]}.

var_str({_, _, Str}) -> Str.
assign_expr(#assign_expr{var_list = VarList,type = AssignSymbol, expresses = ExprElici}, #elici{uid = Uid, indent = Indent, bindings = BindingMaps, content = Content} = RecElici) ->
	ExprStr = elici_util:concat_r(ExprElici#elici.content),
	%%io:format("~nAssign expr: ~p", [ExprStr]),
	case VarList of
		VarList when is_list(VarList) ->
			NewVarList = [var_str(Var) || Var <- VarList],
			%io:format("~nassign expr : ~p", [NewVarList]),
			Str = elici_util:concat(Indent, ["var V",integer_to_list(Uid), atom_to_list(AssignSymbol), ExprStr, ";\n"]),
			{ReStr, _, NewBindingMaps} =
			lists:foldl(fun(Var, {Acc, N, CurBindingMaps}) ->
				{ok, DefineVar, NextBindingMaps} = get_var_define(Var,  CurBindingMaps),
				VarStr = elici_util:concat(Indent, [DefineVar ,Var, " =V", integer_to_list(Uid), "[", integer_to_list(N), "];\n"]),
				{[VarStr| Acc], N+1, NextBindingMaps}
			            end, {[], 0, BindingMaps}, NewVarList),
			RecElici#elici{content = lists:append(ReStr, [Str|Content]), uid = Uid + 1, bindings = NewBindingMaps};
			%%io:format("~nAssign:~p", [Var]),
		_ ->
			{ok, DefineVar, NewBindingMaps} = get_var_define(VarList,  BindingMaps),
			Str = elici_util:concat(Indent, [DefineVar, var_str(VarList), atom_to_list(AssignSymbol), ExprStr]),
			RecElici#elici{content = [Str|Content], bindings = NewBindingMaps}
	end.

bool_expr(#bool_expr{type = BoolType}, #elici{content = Content} = RecElici) ->
	BoolStr = case BoolType of
				  '&&' -> ?ANDAND; %%由于&&在re:replace中有特殊含义,所以需要将其用特殊占位符替代,统一在end_translate中处理
				  '||' -> "||";
				  _ -> atom_to_list(BoolType)
	          end,
	RecElici#elici{content = [BoolStr | Content]}.

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


expr(#expr{type = Type, clause = Clause},#elici{scriptid = ScriptID, content = Content} = RecElici) ->
	ExprStr =
	case Type of
		var ->
			{var, _, Var} = Clause,
			%%ReBindingVar = elici_binds:eval_binds(Clause, BindingMaps),
			%%io:format("expr rebind var :~p, BindMaps:~p", [ReBindingVar, BindingMaps]),
			Var;
		integer ->
			integer_to_list(Clause);
		float ->
			io_lib:format("~w", [Clause]);
		string ->
			Clause;
		atom ->
			["\"", list_to_binary(atom_to_list(Clause)), "\""];
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
	FunTemplate = "%%INST%%.%%FUN%%(%%ARGS%%)",
	LocalTemplate = "%%FUN%%(%%ARGS%%)",
	ArgsStr = elici_util:concat_r(ArgsEilci#elici.content),
	Arity = erlang:length(string:tokens(ArgsStr, ",")),

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
			case elici:get_module_csharp_map(Module) of
				{ok, Instance}->
					Str = elici_util:re_replace(FunTemplate, ["%%INST%%", "%%FUN%%", "%%ARGS%%"],
					                            [Instance, atom_to_list(FunName), ArgsStr]),
					elici_util:concat(Indent, [Str]);
				_ -> throw(?ERROR({undefined, Module, FunName, Arity}))
			end
	end,
	%%io:format("call indent===>~p:~s", [Indent, CallStrBegin]),
	RecElici#elici{content = [CallStr | Content], indent = 0 }.

args(#args{other = Other}, #elici{content = Content} = RecElici) ->
	case Other of
		[] -> RecElici;
		_ -> RecElici#elici{content = ["," | Content]}
	end.
list(#list{elements =  #elici{content = ListContent}}, #elici{content = Content} = RecElici) ->
	ElementStr = elici_util:concat_r(ListContent),
	ListStr = elici_util:concat(["", ElementStr, ""]),
	RecElici#elici{content = [ListStr | Content]}.

elements(#elements{other_elements = OtherElements}, #elici{content = Content} = RecElici) ->
	case OtherElements of
		[] -> RecElici;
		_ -> RecElici#elici{content = ["," | Content]}
	end.

get_var_define(Var, RebindingMap) ->
	case elici_binds:check_double_assign(Var, RebindingMap) of
		{ok, NewRebindingMap1} ->
			{ok, "", NewRebindingMap1};
		{fail, NewRebindingMap2} ->
			{ok, "var ", NewRebindingMap2}
	end.
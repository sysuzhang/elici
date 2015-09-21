%%%-------------------------------------------------------------------
%%% @author Rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc
%%%
%%% @end
%%% Created : 13. 八月 2015 22:43
%%%-------------------------------------------------------------------
-module(elici_binds).
-author("Rongjie").

%% API
-export([eval_binds/2, rebind_var/2, parallel_assign/2, check_double_assign/2]).

-record(rec_var, {var, count = 0}).
%% @doc SSA(Static single assignment form)变量演算:将单次赋值变量转换为可变变量
%% @doc : 参考: https://en.wikipedia.org/wiki/Static_single_assignment_form
-spec eval_binds(Var :: string | tuple(), BindingMaps :: map()) ->
	{ok, RebindVar :: string(), NewBindingMaps :: map()}.
eval_binds({var, _, Var}, BindingMaps) ->
	eval_binds(Var, BindingMaps);
eval_binds(Var, BindingMaps) ->
	case maps:find(Var, BindingMaps) of
		{ok, #rec_var{count = Count}} -> string:concat(Var, integer_to_list(Count));
		_ -> Var
	end.

-spec rebind_var(Var :: string(), BindingMaps :: map()) ->
	{ok, RebindVar :: string, NewBindingMaps :: map()}.
rebind_var({var, _, Var}, BindingMaps) ->
	rebind_var(Var, BindingMaps);
rebind_var(Var, BindingMaps) when is_list(Var) ->
	case maps:find(Var, BindingMaps) of
		{ok, #rec_var{count = Count} = RecVar} ->
			NewCount = Count + 1,
			{ok, string:concat(Var, integer_to_list(NewCount)),
			 maps:put(Var, RecVar#rec_var{count = NewCount},
			          BindingMaps)};
		_ ->
			{ok, string:concat(Var, "1"), maps:put(Var, #rec_var{var = Var, count = 1}, BindingMaps)}
	end.


%% @doc 多变量演算 : 关注:演算顺序
%% @doc 参考: https://en.wikipedia.org/wiki/Assignment_(computer_science)
parallel_assign(VarList, BindingMaps) ->
	{NewVarList, NewBindingMaps} =
	     lists:foldl(fun(AccVar, {AccVarList, AccBinds}) ->
		     {ok, NewVar, NewAccBinds} =  rebind_var(AccVar, AccBinds),
		     {[NewVar | AccVarList ], NewAccBinds}
	                 end,
	                 {[], BindingMaps}, VarList),
	{ok, lists:reverse(NewVarList), NewBindingMaps}.

-spec check_double_assign(Var :: string(), BindingMaps :: map()) ->
	{ok, NewBindingMaps :: map()} | {fail, NewBindingMaps :: map}.
check_double_assign({var, _, Var}, BindingMaps) ->
	check_double_assign(Var, BindingMaps);
check_double_assign(Var, BindingMaps) ->
	case maps:find(Var, BindingMaps) of
		{ok, #rec_var{}} ->
			{ok, BindingMaps};
		_ ->
			{fail, maps:put(Var, #rec_var{var = Var}, BindingMaps)}
	end.











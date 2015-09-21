%%%-------------------------------------------------------------------
%%% @author rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc 语法分析器
%%%
%%% @end
%%% Created : 28. 七月 2015 21:32
%%%-------------------------------------------------------------------
-module(elici_syntax).
-author("rongjie").

%% API
-export([parser/1]).


parser(Tokens) ->
	case elici_parser:parse(Tokens) of
		{ok, Forms} -> {ok, Forms};
		{error, Err} -> Err
	end.

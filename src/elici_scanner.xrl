%%%-------------------------------------------------------------------
%%% @author Rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc leex for elici language
%%%
%%% @end
%%%-------------------------------------------------------------------

Definitions.

UpperCase = [A-Z]
LowerCase = [a-z]
Letter = [A-Za-z_]
Digit = [0-9]
Float = (\+|-)?[0-9]+\.[0-9]+((E|e)(\+|-)?[0-9]+)?
HEX = 0x[0-9A-Fa-f]+
Whitespace  = ([\000-\s]|%.*)
DoubleQuoted = "(\\\^.|\\.|[^\"])*"
SingleQuoted = '(\\\^.|\\.|[^\'])*'
LineComment = //.*
BlockComment = /\*([^\*]|\*[^/])*\*/
KeyWord = (if|else|while|wait|script|return|function|this)
Symbol = (\(|\)|\[|\]|\{|\}|,|;|=|>|<|>=|<=|\+|-|\*|/|\.|!|::|&&|==|!=|\|\|\*\*|\+=|-=|\*=|/=|\*\*=)

Rules.

%% Numbers
{Float}         :build_float(TokenChars, TokenLine).
-?{Digit}+      : build_integer(TokenChars, TokenLine).
{HEX} : build_hex(TokenChars, TokenLine).

{KeyWord} : build_keyword(TokenChars, TokenLine).

%% Variable
{UpperCase}({Letter}|{Digit})* : {token, {var, TokenLine, TokenChars}}.

%% Strings
{DoubleQuoted} : build_string(TokenChars, TokenLine, TokenLen).
{SingleQuoted} : build_string(TokenChars, TokenLine, TokenLen).

%% Atoms
{LowerCase}({Letter}|{Digit})* : build_atom(TokenChars, TokenLine).
{SingleQuoted}{Letter}+{SingleQuoted} : build_quoted_atom(TokenChars, TokenLine, TokenLen).

%% Symbol
{Symbol} : {token, {list_to_atom(TokenChars),TokenLine}}.


%% Ignored
{LineComment} : skip_token.
{BlockComment} : skip_token.
{Whitespace}+ : skip_token.

Erlang code.
-include("elici_define.hrl").

build_integer(Chars, Line) ->
  {token, #integer{line = Line, value =  list_to_integer(Chars)}}.
build_float(Chars, Line) ->
  {token, #float{line = Line, value =  list_to_float(Chars)}}.
build_hex(Chars, Line) ->
  {token, #integer{line = Line, value =  hex_to_int(Chars)}}.

build_keyword(TokenChars, TokenLine) ->
    {token, {list_to_atom(TokenChars), TokenLine}}.

build_string(Chars, Line, Len) ->
  String = unescape_string(strip(Chars, Len)),
  {token, {string, Line, "\"" ++ String ++ "\""}}.

build_atom(Chars, Line) ->
  {token, {atom, Line, list_to_atom(Chars)}}.

build_quoted_atom(Chars, Line, Len) ->
  String = lists:sublist(Chars, 2, Len - 2),
  build_atom(String, Line).



unescape_string(String) -> unescape_string(String, []).

unescape_string([], Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Output) ->
  Char = case Escaped of
    $\\ -> $\\;
    $/  -> $/;
    $\" -> $\";
    $\' -> $\';
    $b  -> $\b;
    $d  -> $\d;
    $e  -> $\e;
    $f  -> $\f;
    $n  -> $\n;
    $r  -> $\r;
    $s  -> $\s;
    $t  -> $\t;
    $v  -> $\v;
    _   -> throw({error, {"unrecognized escape sequence: ", [$\\, Escaped]}})
  end,
  unescape_string(Rest, [Char|Output]);
unescape_string([Char|Rest], Output) ->
  unescape_string(Rest, [Char|Output]).

strip(TokenChars,TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).

hex_to_int([_,_|R]) ->
    {ok,[Int],[]} = io_lib:fread("~16u", R),
    Int.
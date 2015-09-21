%%%-------------------------------------------------------------------
%%% @author Rongjie
%%% @copyright (C) 2015, <Alibaba Mobile>
%%% @doc yecc文件
%%%      用于语法分析
%%%
%%% @end
%%%-------------------------------------------------------------------

%%非终结符
Nonterminals
   grammar  script_meta
   module_meta functions function_define
   statements  if_statement wait_statement return_statement statement
   if_clause else_clause %%else_if_clauses else_if_clause
   expresses assign_expr
   bool_expr comp_expr math_expr mult_expr unary_expr  pow_expr expr
   call_expr
   args list elements
   rebind_op bool_op comp_op add_op mult_op pow_op unary_op.

%终结符
Terminals '+' '-' '*' '/' '**' '.'
      '=' '+=' '-=' '*=' '/=' '**='
      '::' '&&' '||'
      '!'
      '>' '>=' '<' '<=' '==' '!='
      ';' ',' '(' ')' '[' ']' '{' '}'
     'if' 'else'  'wait' 'script' 'return' 'this'
     'function' 'module'
     atom integer float string var.

Rootsymbol grammar.


%%语法分析器
%% 语法: 区分模块与脚本
%% grammar -> '$empty' : [].
grammar ->  module_meta  functions : #grammar{type = module, meta = '$1', statements = '$2'}.    %%模块
grammar ->  script_meta  statements : #grammar{type = script, meta = '$1', statements = '$2'}.   %%脚本
grammar ->  statements                 : #grammar{type = script, meta = #meta{type = script, param = []}, statements = '$1'}.   %%脚本

module_meta -> '-' 'module' '(' atom ')' '.' : #meta{type = module, module = '$4'}. %%标识模块
script_meta -> '-' 'script' '(' '[' args ']' ')' '.'  : #meta{type = script, param = '$5'}.   %%支持参数

%% 段落分析
%% 注意: 1. 先将特殊子句展开
statements -> '$empty' : [].
statements -> if_statement statements : ['$1'|'$2'].
statements -> wait_statement statements : ['$1' | '$2'].
statements -> return_statement statements : ['$1'].
statements -> statement statements :  ['$1' | '$2'].
statements -> function_define statements : store_functions('$1', '$2').


%% 函数定义(注意如何消除shift/reduce冲突的)
%% LALR(1)方法往右多看一个Token,通过这个Token是否有新的规则匹配来决定是做shift还是reduce,
%%        如果刚好有一条规则允许其reduce后也可以工作下去,那么语法分析器就会非常郁闷,不知道应该继续shift还是先reduce再继续
%%    ==> 推论: 如果语法分析器可以再多看一个Token(即向前看两个Token)(LALR(2)) 那么它就可以知道应该shift还是reduce了
%% 解决方法: 1) 发生shift/reduce的语法规则,增加一个terminate的Token，让语法规则继续进行shift操作
%%         比如: function myfun() {}  function myfun2() {}  这样的两个结构样的子句,语法分析器在看到第二个function这个Token时,会不知道应该shift还是reduce
%%               因为: 多看一个function来说,它既可以立即reduce,然后用递归语义继续进行shift,也不影响语法分析器继续工作下去
%%                    也可以立即shift这个Token,然后继续进行语法分析,
%%               所以: 如果加多看的Token,这个Token不在任何一条语法规则中,那么当语法分析器向前多看的Token时,它就只能继续做shift操作,才能保证语法分析尽可能成立
%%
%%         2) 重建语法规则: 调整语法规则的边界,让Terminate成为Nonterminate的边界,这样可以递归定义Nonterminate
%%            举例: if/else的语法就可以通过重新分拆(careful study the structure)其结构来处理(本质还是去发现可以界定shift还是reduce操作的Terminate的Token)
functions ->  function_define : ['$1'].
functions ->  function_define  functions  : ['$1' | '$2'].
function_define -> 'function' atom '(' args ')' '{' statements '}' : [#function{line = ?line('$1'), name = unwrap('$2'), args = '$4', body= '$7'}].


%% if表达式语句(需要注意shift/reduce冲突)
if_statement -> if_clause : #if_statement{if_clause = '$1'}.
if_statement -> if_clause else_clause : #if_statement{if_clause = '$1', else_clause = '$2'}.
if_statement -> if_clause 'else' if_statement: #if_statement{if_clause = '$1', else_clause = '$3'}.

if_clause -> 'if' '(' expresses ')' '{' statements '}' : #if_clause{conditions = '$3', statements = '$6'}.
else_clause -> 'else' '{' statements '}' : '$3'.

wait_statement -> atom '::' 'wait' '(' args  ')' ';' :  #wait_statement{mod = unwrap('$1'), args = '$5', statements = []}.

return_statement -> 'return' statement  : #return_statement{clause = '$2'}.


%% 语句分析
statement -> expresses ';'  : #statement{type = expresses, clause = '$1'}.

%%表达式
expresses ->  bool_expr :  #expresses{type = bool_expr,
                                      exprs = '$1'}.
expresses ->  assign_expr : #expresses{type = assign_expr,
                                       exprs = '$1'
                                       }.

bool_expr -> bool_expr  bool_op comp_expr
                : #bool_expr{line = ?line('$2'),
                               type = ?op('$2'),
                               left = '$1',
                               right = '$3'}.
bool_expr -> comp_expr : '$1'.


comp_expr -> math_expr comp_op math_expr
             : #comp_expr{line = ?line('$2'),
                            type = ?op('$2'),
                            left = '$1',
                            right = '$3'}.
comp_expr -> math_expr : '$1'.

assign_expr -> var rebind_op expresses
             : #assign_expr{line = ?line('$2'),
                            type = ?op('$2'),
                            var_list = '$1',
                            expresses = '$3' }.

%% 特殊语法: 多变量同时赋值(绝对不要认为是Erlang的列表List)
assign_expr -> list rebind_op expresses
             : #assign_expr{line = ?line('$2'),
                            type = ?op('$2'),
                            var_list = check_parallel_rebind('$1'),
                            expresses = '$3' }.





%% 数学表达式
math_expr -> math_expr add_op mult_expr
             : #math_expr{line = ?line('$2'),
                            type = ?op('$2'),
                            left = '$1',
                            right = '$3'}.
math_expr -> mult_expr : '$1'.

mult_expr -> mult_expr mult_op unary_expr
             : #mult_expr{line = ?line('$2'),
                            type = ?op('$2'),
                            left = '$1',
                            right = '$3'}.
mult_expr -> unary_expr : '$1'.

unary_expr -> unary_op unary_expr
             : #unary_expr{line = ?line('$2'),
                           type = ?op('$1'),
                           exprs = '$2'}.

unary_expr -> pow_expr : '$1'.

pow_expr -> expr pow_op pow_expr
            : #pow_expr{line = ?line('$2'),
                           type = ?op('$2'),
                           left = '$1',
                           right = '$2'}.
pow_expr -> expr : '$1'.


%% 最终的表达式,优化级最高
expr -> integer : #expr{type = integer, clause = unwrap('$1')}.
expr -> float : #expr{type = float, clause = unwrap('$1')}.
expr -> string :#expr{type = string, clause = unwrap('$1')}.
expr -> atom :  #expr{type = atom, clause = unwrap('$1')}.
expr -> list:  #expr{type = list, clause ='$1'}.
expr -> call_expr :  #expr{type = call, clause = '$1'}.
expr -> var :   #expr{type = var, clause = '$1'}. %%务必标识,需要做SSA
expr -> 'this' : #expr{type = this, clause = '$1'}.
expr -> '(' math_expr ')': #expr{type = priority, clause = '$2'}.


%%list支持待定


%% 调用
call_expr -> atom '(' args ')' : #call_expr{type = local,
                                            function = unwrap('$1'),
                                            args = '$3'}.
call_expr -> atom '::' atom '(' args ')' : #call_expr{type = remote,
                                                      module = unwrap('$1'),
                                                      function = unwrap('$3'),
                                                      args = '$5'}.   %%支持命名空间


%% 参数
%% 注意: 为了支持函数变参, 语法规则规定,只允许最后一个参数使用List
%%      另一个原因是:需要支持一套语法规则转C#代码,C#的变参只允许放在参数表的最后一个参数
args -> '$empty' : [].
args -> expresses : [#args{type = expresses, clause = '$1'}].
args -> expresses ',' args : [#args{type = expresses, clause = check_args('$1')}| '$3'].

%% 可变参数(比较特殊,重写全部规则:绝对不是为了实现Erlang的列表,而是为了支持变参!)
list -> '[' ']' : [].
list -> '[' elements ']' : '$2'.

elements -> expresses : ['$1'].
elements -> expresses ',' elements : ['$1' | '$3'].



%% 赋值运算
rebind_op -> '='   : '$1'. %%atom_to_list(unwrap('$1')).
rebind_op -> '+='  : '$1'. %%atom_to_list(unwrap('$1')).
rebind_op -> '-='  : '$1'. %%atom_to_list(unwrap('$1')).
rebind_op -> '*='  : '$1'. %%tom_to_list(unwrap('$1')).
rebind_op -> '/='  : '$1'. %%atom_to_list(unwrap('$1')).
rebind_op -> '**=' : '$1'. %%atom_to_list(unwrap('$1')).

%% 布尔运算
bool_op -> '&&' : '$1'.
bool_op -> '||'  : '$1'.

%% 比较运算
comp_op -> '==' : '$1'.
comp_op -> '!=' : '$1'.
comp_op -> '>'  : '$1'.
comp_op -> '<'  : '$1'.
comp_op -> '>=' : '$1'.
comp_op -> '<=' : '$1'.

%% 加法运算
add_op -> '+'  : '$1'.
add_op -> '-'  : '$1'.

%% 算法运算
mult_op -> '*' : '$1'.
mult_op -> '/' : '$1'.
%% mult_op -> '%' : '$1'.

%%指数运算
pow_op -> '**' : '$1'.

%%一元运算
unary_op -> '+'   : '$1'.
unary_op -> '-'   : '$1'.
unary_op -> '!'   : '$1'.


Erlang code.
-include("elici.hrl").
-import(lists, [append/2]).

-define(line(Node), element(2, Node)).
-define(op(Node), element(1, Node)).


unwrap({_,_,V}) -> V;
unwrap({V,_}) -> V.

store_functions(Function, Statements) ->
    case get(functions) of
        undefined -> put(functions, [Function]);
        Functions -> put(functions, [Function | Functions])
    end,
    Statements.


%% 并行变量绑定只允许变量
check_parallel_rebind(VarList) ->
    %%io:format("~nVarList:~p", [VarList]),
    check_parallel_rebind(VarList, []).

check_parallel_rebind([], Checked) ->
    lists:reverse(Checked);

check_parallel_rebind([Var|Other], Checked) ->
    #expresses{type = bool_expr, exprs = Expr} = Var,
    #expr{type = var, clause = VarTokens} = Expr,
    case element(1, VarTokens) =:= var of
        true -> check_parallel_rebind(Other, [VarTokens|Checked]);
        _ -> ?THROW_UNEXPECTED(unexpected_parallel_binding, Var)
    end.

%%TODO 限定参数只能最后一个参数是变参
check_args(Expresses) ->
    Expresses.

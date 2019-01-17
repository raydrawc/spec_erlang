%%----------------------------------------------------
%% @doc 服务端代码gen_server范本，所有gen_server模块请按这个布局和规范来编写
%% 项目标准vim配置下输入<gs>esc可以生成一份标准模板
%% 以@doc 开始著名模块的作用和意图, 空行后紧接@author 开始作者前面，最后以@end来收尾
%% 
%% @author Jangee@qq.com
%% @end
%%----------------------------------------------------
-module(spec_gen_server).
-behaviour(gen_server).
-export([   %% --- 注明模块名和behaviour属性后紧接是导出的接口列表，每个模块应该导出尽可能少的函数
        start_link/0
    ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]). %% --- OTP接口单独一个export来区分，这个一般比较少动的

%% 空行后紧接的是包含的头文件
-include("common.hrl"). %% --------- 一般模块尽可能导入该头文件，并且是第一个
-include("spec_erl.hrl"). %% ------- 本功能专属头文件尽量在最后

%% 如果有私有宏则在空行后开始
-define(SPEC_ERL_STATE_VER, 1). %% ---- 功能专用宏必须以模块名作为前缀如这个STATE_VER因为模块名是SPEC_ERL所以要写成SPEC_ERL_STATE_VER（宏鼓励用全大写方式）

%% 如果有record的定义，则在宏后面开始,这样可以直接调用上面定义的宏作为默认值
-record(state,{
        ver = ?spec_erl_state_ver
        ,val = 0 :: non_neg_integer() %% ---- record内部最好是第二行开始以逗号开始的写法，顺便spec一下期望的值类型
    }).

%%----------------------------------------------------
%% 外部接口 ---（外部接口分割线，项目标准vim配置下输入<%%>esc则可以生成这两条线的注释)
%%----------------------------------------------------
%% @doc 外部接口注释由@doc 开始方便阅读代码时区分
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%----------------------------------------------------
%% OTP apis --- （OTP标准接口分割线）
%%----------------------------------------------------

init([]) ->
    do_init().

%% OTP接口的注释参考内部函数，不以@doc 开始
handle_call(hello, _From, State) ->
    {reply, hello, State}.

handle_call(_Request, _From, State) ->
    ?DEBUG("无效请求 ~w", [_Request]), %% --- 未知消息一律要做容错并且打印出来
    {reply, false, State}. %% --- handle_call的容错返回必须是{reply, false, State} 以免caller方timeout或者判断错误

handle_cast(_Msg, State) ->
    ?DEBUG("无效请求 ~w", [_Msg]), %% --- 未知消息一律要做容错并且打印出来
    {noreply, State}.

handle_info(_Info, State) ->
    ?DEBUG("无效消息 ~w", [_Info]), %% --- 未知消息一律要做容错并且打印出来
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------
%% 内部私有 --- （私有函数分割线）
%%----------------------------------------------------
%% 私有函数注释不要以@doc开始
do_init() ->
    ?INFO("开始启动..."), %%  --- 常驻随机启动进程要加上友好的启动信息
    State = #state{},
    ?INFO("启动完成"), %% --- 启动完成也打印提示一下以便追踪系统启动过程
    {ok, State}.


%%----------------------------------------------------
%% 测试用例 -- （测试用例分割线)
%%----------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST). %% --- 测试用例统一用包含eunit来做单元测试，并嵌入到-ifdef(TEST).内.
%% 测试内接口
test_init() ->
    do_init().

-endif.


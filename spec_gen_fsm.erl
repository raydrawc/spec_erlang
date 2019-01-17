%%----------------------------------------------------
%% @doc 服务端代码gen_fsm范本，所有gen_fsm模块请按这个布局和规范来编写
%% 项目标准vim配置下输入<gs>esc可以生成一份标准模板
%% 以@doc 开始著名模块的作用和意图, 空行后紧接@author 开始作者前面，最后以@end来收尾
%% 
%% @author Jangee@qq.com
%% @end
%%----------------------------------------------------
-module(spec_gen_server).
-behaviour(gen_fsm).
-export([   %% --- 注明模块名和behaviour属性后紧接是导出的接口列表，每个模块应该导出尽可能少的函数
        start_link/1
    ]
).
-export(    %% --- 状态机的状态回调单独一个export以便可以快速区分
    [
        ,idle/2
        ,active/2
    ]
).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).


%% 空行后紧接的是包含的头文件
-include("common.hrl"). %% --------- 一般模块尽可能导入该头文件，并且是第一个
-include("spec_erl.hrl"). %% ------- 本功能专属头文件尽量在最后

%% 如果有私有宏则在空行后开始
-define(SPEC_ERL_STATE_VER, 1). %% ---- 功能专用宏必须以模块名作为前缀如这个STATE_VER因为模块名是SPEC_ERL所以要写成SPEC_ERL_STATE_VER（宏鼓励用全大写方式）

%% 如果有record的定义，则在宏后面开始,这样可以直接调用上面定义的宏作为默认值
-record(state,{
        ver = ?spec_erl_state_ver
        ,val = 0 :: non_neg_integer() %% ---- record内部最好是第二行开始以逗号开始的写法，顺便spec一下期望的值类型
        ,timeout = 0 :: non_neg_integer() %% --- 状态机的state里面尽量定义这个倒计时字段以供continue时使用
        ,ts = {0, 0, 0} :: tuple() %% --- timeout的基准时间，R18B版本之前通过erlang:now()获得
    }).

%%----------------------------------------------------
%% 外部接口 ---（外部接口分割线，项目标准vim配置下输入<%%>esc则可以生成这两条线的注释)
%%----------------------------------------------------
%% @doc 外部接口注释由@doc 开始方便阅读代码时区分
start_link([])->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

%%----------------------------------------------------
%% 活动状态 ---（状态回调分割线，在外部接口后面)
%%----------------------------------------------------
%% OTP接口的注释参考内部函数，不以@doc 开始
idle(timeout, State = #state{}) ->
    State1 = State#state{
    },
    continue(active, State). %% --- 状态回调接口返回值也一律由continue函数来接管
idle(_, State) -> %% --- 状态回调接口一样要做好容错处理
    continue(idle, State).

%% OTP接口的注释参考内部函数，不以@doc 开始
active(timeout, State = #state{}) ->
    State1 = State#state{
    },
    continue(idle, State). %% --- 状态回调接口返回值也一律由continue函数来接管
active(_, State) -> %% --- 状态回调接口一样要做好容错处理
    continue(active, State).

%%----------------------------------------------------
%% OTP apis --- （OTP标准接口分割线）
%%----------------------------------------------------

init([])->
    do_init().

%% OTP接口的注释参考内部函数，不以@doc 开始
handle_event(_Event, StateName, State) ->
    ?DEBUG("无效事件 ~w", [_Event]), %% --- 未知消息一律要做容错并且打印出来
    continue(StateName, State). %% --- 状态机的OTP接口返回值一律由continue函数来接管

handle_sync_event(_Event, _From, StateName, State) ->
    ?DEBUG("无效请求 ~w", [_Event]), %% --- 未知消息一律要做容错并且打印出来
    Reply = false, %% --- handle_sync_event的容错返回必须是false,  以免caller方timeout或者判断错误
    continue(Reply, StateName, State). %% --- 状态机的OTP接口返回值一律由continue函数来接管（注意contine有提供带reply版本）

handle_info(_Info, StateName, State) ->
    ?DEBUG("无效消息 ~w", [_Info]), %% --- 未知消息一律要做容错并且打印出来
    continue(StateName, State). %% --- 状态机的OTP接口返回值一律由continue函数来接管

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%----------------------------------------------------
%% 内部私有 --- （私有函数分割线）
%%----------------------------------------------------
%% 私有函数注释不要以@doc开始
do_init() ->
    ?INFO("开始启动..."), %%  --- 常驻随机启动进程要加上友好的启动信息
    Timeout = 300000,
    State = #state{
        ts = erlang:now()
        ,timeout = Timeout
    },
    ?INFO("启动完成~w ~w", [StateName, Timeout]), %% --- 状态机的启动完成信息里面最好要带上当前的StateName和timeout
    {ok, idle, State, Timeout}.

%% 继续下一个状态（带倒计时的状态机统一定义这个时间校准函数）
continue(StateName, State = #state{timeout = infinity}) ->
    {next_state, StateName, State, infinity};
continue(StateName, State = #state{ts = Ts, timeout = Timeout}) ->
    {next_state, StateName, State, util:time_left(Timeout, Ts)}.

continue(Reply, StateName, State = #state{timeout = infinity}) ->
    {reply, Reply, StateName, State, infinity};
continue(Reply, StateName, State = #state{ts = Ts, timeout = Timeout}) ->
    {reply, Reply, StateName, State, util:time_left(Timeout, Ts)}.

%%----------------------------------------------------
%% 测试用例 -- （测试用例分割线)
%%----------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST). %% --- 测试用例统一用包含eunit来做单元测试，并嵌入到-ifdef(TEST).内.
%% 测试内接口
test_init() ->
    do_init().

-endif.


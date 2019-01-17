%%----------------------------------------------------
%% @doc 服务端代码普通模块coding范本，所有普通模块请按这个布局和规范来编写
%% 项目标准vim配置下输入<erl>esc可以生成一份标准模板
%% 以@doc 开始著名模块的作用和意图, 空行后紧接@author 开始作者前面，最后以@end来收尾
%% 
%% @author Jangee@qq.com
%% @end
%%----------------------------------------------------
-module(spec_erl).
-export([   %% --- 注明模块名属性后紧接是导出的接口列表，每个模块应该导出尽可能少的函数
        init/0
    ]).

%% 空行后紧接的是包含的头文件
-include("common.hrl"). %% --------- 一般模块尽可能导入该头文件，并且是第一个
-include("spec_erl.hrl"). %% ------- 本功能专属头文件尽量在最后

%% 如果有私有宏则在空行后开始
-define(SPEC_ERL_VER, 1). %% ---- 功能专用宏必须以模块名作为前缀如这个VER因为模块名是SPEC_ERL所以要写成SPEC_ERL_VER（宏鼓励用全大写方式）

%% 如果有record的定义，则在宏后面开始,这样可以直接调用上面定义的宏作为默认值
-record(spec_erl,{
        ver = ?spec_erl_ver
        ,val = 0 :: non_neg_integer() %% ---- record内部最好是第二行开始以逗号开始的写法，顺便spec一下期望的值类型
    }).

%%----------------------------------------------------
%% 外部接口 ---（外部接口分割线，项目标准vim配置下输入<%%>esc则可以生成这两条线的注释)
%%----------------------------------------------------
%% @doc 外部接口注释由@doc 开始方便阅读代码时区分
-spec init() -> {ok, #spec_erl{}}. %% 接口spec这个是推荐项，非必选项
init() ->
    do_init().

%%----------------------------------------------------
%% 内部私有 --- （私有函数分割线）
%%----------------------------------------------------
%% 私有函数注释不要以@doc开始
do_init() ->
    SpecErl = #spec_erl{}, %% --- erlang代码除了变量名要用大小（不带下划线）之外，其他一律用小写加下划线方式
    {ok, SpecErl}.


%%----------------------------------------------------
%% 测试用例 -- （测试用例分割线)
%%----------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST). %% --- 测试用例统一用包含eunit来做单元测试，并嵌入到-ifdef(TEST).内.
%% 测试内接口
test_init() ->
    do_init().

-endif.

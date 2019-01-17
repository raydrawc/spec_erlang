# bitstring注意事项
 * @author Jange

----

 1. 返回给客户端中文字符串全部用宏 ?T()抱起来：?T("中文")
 2. 要匹配bitstring中的中文：
    case Bin of
        <<"中文"/utf8, _/binary>> -> ok;
        _ -> no
    end.
 3. 裸写中文字符串: Str = <<"中文"/utf8>> 而不是 Str = <<"中文">>
 4. 字符串连接 不能用 ++ 操作符
    do not : Str1 ++ Str2
    do : <<Str1/binary, Str2/binary>>
    do not : lists:concate([Bin1, Bin2])
    do : bitstring:concate([Bin1, Bin2])
 5. 原来的string模块只能处理string(list)不能处理bitstring，
    项目里增加了一个bitstring模块提供类string的接口（不完整版本，有需求再补充）
 6. 空字符的默认值用<<>>而不是原来的 ""
 7. 看字符串的中文长度可以用util:utf8_len/1而不能直接用erlang:length/1
 8. 从一个大的bitstring里面分割一个字符串（或者二进制数据)出来保存要用copy
    LongBin = <<ShortBin:64, _/binary>>
    上面这行代码的里的ShortBin如果要保存到ets的话，要用ShortBin1 = binary:copy(ShortBin)
    否则 binary:referenced_byte_size(ShortBin) =:= erlang:byte_size(LongBin)
    这时会导致LongBin一直不会被GC回收

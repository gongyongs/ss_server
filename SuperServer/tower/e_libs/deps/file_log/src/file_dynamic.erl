-module(file_dynamic).
-author('erlangonrails@gmail.com').
-export([new/1,
         add_fun/3,
         compile/1]).

-record(file_meta_mod, {module, 
                        exports = [], 
                        forms = []}).

-spec new(ModName :: atom()) -> #file_meta_mod{}.
new(ModName) when is_atom(ModName) ->
    #file_meta_mod{module = ModName}.


-spec add_fun(MetaMod :: #file_meta_mod{}, Fun :: string(), Export :: boolean()) ->
    {ok, #file_meta_mod{}} | {error, 'parse_fun_forms_error'}.
add_fun(MetaMod, Fun, Export) when is_list(Fun) ->
    case parse_fun_string(Fun) of
        {ok, Form} ->
            add_fun(MetaMod, Form, Export);
        Err ->
            Err
    end;
add_fun(MetaMod, {function, _Line, FuncName, Arity, _Clauses} = Form, true) -> 
    {ok,  MetaMod#file_meta_mod{exports = [{FuncName, Arity} | MetaMod#file_meta_mod.exports],
                                 forms = [Form | MetaMod#file_meta_mod.forms]}};
add_fun(MetaMod, {function, _Line, _FuncName, _Arity, _Clauses} = Form, false) ->
    {ok, MetaMod#file_meta_mod{forms = [Form | MetaMod#file_meta_mod.forms]}};
add_fun(_, _, _) ->
    {error, 'parse_fun_forms_error'}.


-spec compile(MetaMod :: #file_meta_mod{}) -> ok | {error, term()}.
compile(MetaMod) ->
    compile(MetaMod, [report_errors, report_warnings, return_errors]).

compile(MetaMod, Options) ->
    Forms = [{attribute, 2, module, MetaMod#file_meta_mod.module},
             {attribute, 3, export, MetaMod#file_meta_mod.exports}],
    FileName = MetaMod#file_meta_mod.module,

    Forms1 = [{attribute, 1, file, {atom_to_list(FileName), 1}} | Forms],
    Forms2 = Forms1 ++ lists:reverse(MetaMod#file_meta_mod.forms),
    
    case compile:forms(Forms2, Options) of
        {ok, Module, Bin} ->
            code:purge(Module),
            case code:load_binary(Module, atom_to_list(Module) ++ ".erl", Bin) of
                {module, _Module} -> ok;
                _ -> {error, 'code_load_binary_error'}
            end;
        _ -> 
            {error, 'compile_forms_error'}
    end.


%% Internal APIs:
-spec parse_fun_string(Fun :: string()) ->
    {ok, term()} | {error, term()}.
parse_fun_string(Fun) ->
    case erl_scan:string(Fun) of
        {ok, Toks, _} ->
            case erl_parse:parse_form(Toks) of
                {ok, _Form} = Res ->
                    Res;
                _Err ->
                    {error, 'parse_fun_forms_error'}
            end;
        _Err ->
            {error, 'parse_fun_forms_error'}
    end.

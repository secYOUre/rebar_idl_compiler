%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Alfonso De Gregorio (adg@secYOUre.com), 
%%                    Dave Smith          (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
%% The rebar_idl_compiler module is a plugin for rebar that compiles
%% OMG IDL specifications. By default, it compiles all idl/*.idl
%% to src/backends/<idl_backend_name>/*.erl.
%%
%% Configuration options should be placed in rebar.config under
%% 'idl_opts'.  Available options are documented at ic man page
%% <http://www.erlang.org/doc/man/ic.html> and include, but are 
%% not limited to:
%%
%%  outdir:  places all output files in the directory given by 
%%           the option. The directory will be created if it does not 
%%           already exist. Defaults to: src/backends/<backend_name>
%%           where the backend name is the identifier specified via
%%           the 'be' option. See below.
%%           Example option: {outdir, "output/generated"}
%%
%%  cfgfile: Uses FileName as configuration file. Options will override
%%           compiler defaults but can be overridden by command line 
%%           options. Default value is ".ic_config"
%%           Example option: {cfgfile, "special.cfg"}
%%
%%  be:      Determine which back-end IC will generate code for. If left
%%           out, erl_corba will be used. Currently, IC support the 
%%           following back-ends:
%%
%%              erl_corba
%%              This option switches to the IDL generation for CORBA.
%%
%%              erl_template
%%              Generate CORBA call-back module templates for each 
%%              interface in the target IDL file. Note, will overwrite 
%%              existing files.
%%
%%              erl_plain
%%              Will produce plain Erlang modules which contain 
%%              functions that map to the corresponding interface 
%%              functions on the input file.
%%
%%              erl_genserv
%%              This is an IDL to Erlang generic server generation option.
%%
%%              c_client
%%              Will produce a C client to the generic Erlang server.
%%
%%              c_server
%%              Will produce a C server switch with functionality of a 
%%              generic Erlang server.
%%
%%              java
%%              Will produce Java client stubs and server skeletons with 
%%              functionality of a generic Erlang server.
%%
%%              c_genserv
%%              Deprecated. Use c_client instead.
%%
%%              Example option: {be, erl_genserv}.
%%
%% {{impl, IntfName}, ModName}:
%%           Assumes that the interface with name IntfName is 
%%           implemented by the module with name ModName and will 
%%           generate calls to the ModName module in the server 
%%           behavior. Note that the IntfName must be a fully scoped
%%           name as in "M1::I1".
%%
%%
%%
%%
%% For example, here is a valid idl_opts block for a rebar.config:
%%
%%   {idl_opts, [
%%               {gen_hrl, true}, 
%%               {be, erl_genserv},
%%               {cfgfile, "cfg/server.cfg"},
%%               {{impl, yetanotheridlinterface::sayhi}, modulesayhi}
%%              ]}.
%%
-module(rebar_idl_compiler).
-author('adg@secYOUre.com').

-export([compile/2,
         clean/2]).

-define(FAIL, throw({error, failed})).

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(Config::rebar_config:config(), AppFile::file:filename()) -> 'ok'.
compile(Config, _AppFile) ->
    rebar_base_compiler:run(Config, filelib:wildcard("idl/*.idl"),
                            "idl", ".idl", "idl", ".erl",
                            fun compile_idl/3).

-spec clean(Config::rebar_config:config(), AppFile::file:filename()) -> 'ok'.
clean(Config, _AppFile) ->
    rebar_file_utils:delete_each(idl_generated_files("src", Config)),
    ok.

-spec compile_idl(file:filename(), file:filename(),
                   rebar_config:config()) -> ok.
compile_idl(Source, Target, Config) ->
    ok = rebar_utils:ensure_dir(Target),
    Opts = rebar_config:get(Config, idl_opts, []),
    Backend = get_backend_name(Opts),
    Outdir = proplists:get_value(outdir, Opts, "src/backends/" ++ Backend),
    ok = rebar_utils:ensure_dir(Outdir),
    Opts2 = [{outdir, Outdir} ] ++ Opts,
    case ic:gen(Source, Opts2) of
        ok ->
            ok;
        {error, _Reason} ->
            ?FAIL
    end.

idl_generated_files(SrcDir, Config) ->
    Opts = rebar_config:get(Config, idl_opts, []),
    Backend = get_backend_name(Opts),
    Outdir = proplists:get_value(outdir, Opts, SrcDir ++ "/backends/" ++ Backend),
    rebar_utils:find_files(Outdir, "^.*\$").

get_backend_name(Opts) ->
        atom_to_list(proplists:get_value(be, Opts, erl_corba)).

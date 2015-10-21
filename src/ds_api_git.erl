-module(ds_api_git).

-export([clone/2, tags/1, copy_tag/3]).

clone(RepoURL, ComponentDir) ->
    Command = <<"git clone ", RepoURL/binary, " git">>,
    case ds_api_util:cmd(Command, ComponentDir) of
        {ok, _Reply} -> ok;
        Error        -> Error
    end.

tags(GitDir) ->
    case update_tags(GitDir) of
        ok ->
            Command = <<"git tag">>,
            case ds_api_util:cmd(Command, GitDir) of
                {ok, TagsBin} ->
                    {ok, binary:split(TagsBin, <<$\n>>, [global, trim])};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

copy_tag(GitDir, Tag, OutputDir) ->
    TempFile0 = filename:join(OutputDir, <<"tag.zip">>),
    case filelib:ensure_dir(TempFile0) of
        ok ->
            Command = <<"git archive --format zip -0 -o ", TempFile0/binary, " ", Tag/binary>>,
            case ds_api_util:cmd(Command, GitDir) of
                {ok, _Reply} ->
                    %% zip does not like binary file names...
                    TempFile1 = unicode:characters_to_list(TempFile0),
                    case zip:extract(TempFile1, [{cwd, OutputDir}]) of
                        {ok, _Files} -> file:delete(TempFile0);
                        Error        -> Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

update_tags(GitDir) ->
    Command = <<"git fetch --prune origin +refs/tags/*:refs/tags/*">>,
    case ds_api_util:cmd(Command, GitDir) of
        {ok, _Reply} -> ok;
        Error        -> Error
    end.

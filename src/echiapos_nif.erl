-module(echiapos_nif).

-on_load(init/0).

init() ->
  SoName = case code:priv_dir(echiapos) of
    {error, bad_name} ->
      case code:which(?MODULE) of
        Filename when is_list(Filename) ->
          filename:join([filename:dirname(Filename),"../priv", "echiapos_nif"]);
        _ ->
          filename:join("../priv", "echiapos_nif")
      end;
    Dir ->
      filename:join(Dir, "echiapos_nif")
  end,
  erlang:load_nif(SoName, 0).

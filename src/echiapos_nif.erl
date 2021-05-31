-module(echiapos_nif).

-export([
  encoding_ans_encode_deltas/2,
  encoding_ans_decode_deltas/3
]).

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


encoding_ans_encode_deltas(_Deltas, _R) ->
  erlang:nif_error({error, not_loaded}).

encoding_ans_decode_deltas(_Binary, _NumDeltas, _R) ->
  erlang:nif_error({error, not_loaded}).

-module(prop_ucl).
-include_lib("proper/include/proper.hrl").

-define(SIZE, 60000).

%%%%%%%%%%%%%%%%%%
% Properties
prop_test() ->
    ?FORALL(Bin, resize(?SIZE, binary()),
        begin
            collect(to_range(?SIZE div 10,
                             byte_size(Bin)),
                    ucl(Bin)
                   )
        end).

%%%%%%%%%%%%%%%
% Helpers
ucl(Bin) ->
    case ucl:to_json(Bin) of
        {ok, _} -> true;
        {error, _ } -> true;
        _ -> false
    end.

to_range(M, N) ->
    Base = N div M,
    {Base*M, (Base+1)*M}.

%% -------------------------------------------------------------------
%%
%% stats: Statistics Suite for Erlang
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(basho_stats_sample).

-export([new/0,
         update/2, update_all/2,
         update_with_freq/3, update_all_with_freq/2,
         count/1,
         min/1, mean/1, max/1, sum/1,
         variance/1, sdev/1,
         summary/1]).

-export([to_struct/1, from_struct/1]).
-export([merge/2, scale/2]).

-include("stats.hrl").

-ifdef(EQC).
-export([prop_main/0]).
-endif.

-record(state, { n = 0,
                 min = 'NaN',
                 max = 'NaN',
                 sum  = 0,
                 sum2 = 0 }).


%% ===================================================================
%% Public API
%% ===================================================================

new() ->
    #state{}.

update(Value, State) ->
    State#state {
      n   = State#state.n + 1,
      min = nan_min(Value, State#state.min),
      max = nan_max(Value, State#state.max),
      sum = State#state.sum + Value,
      sum2= State#state.sum2 + (Value * Value)}.


update_with_freq(Value, Count, State) ->
    State#state {
      n   = State#state.n + Count,
      min = nan_min(Value, State#state.min),
      max = nan_max(Value, State#state.max),
      sum = State#state.sum + (Value * Count),
      sum2 = State#state.sum2 + ((Value * Value) * Count)}.

update_all(Values, State) ->
    lists:foldl(fun(Value, S) -> update(Value, S) end,
                        State, Values).

update_all_with_freq(Frequencies, State) ->
    lists:foldl(fun ({Value, Count}, S) -> update_with_freq(Value, Count, S) end,
                State, Frequencies).


to_struct(State) ->
    {[{<<"n">>, State#state.n},
      {<<"min">>, State#state.min},
      {<<"max">>, State#state.max},
      {<<"sum">>, State#state.sum},
      {<<"sum2">>, State#state.sum2}
     ]}.

from_struct({S}) ->
    N    = ensure_number(proplists:get_value(<<"n">>, S)),
    Min  = ensure_number(proplists:get_value(<<"min">>, S)),
    Max  = ensure_number(proplists:get_value(<<"max">>, S)),
    Sum  = ensure_number(proplists:get_value(<<"sum">>, S)),
    Sum2 = ensure_number(proplists:get_value(<<"sum2">>, S)),

    #state{n    = N,
           min  = Min,
           max  = Max,
           sum  = Sum,
           sum2 = Sum2}.

merge(#state{} = A, #state{} = B) ->
    #state{n = A#state.n + B#state.n,
           min = nan_min(A#state.min, B#state.min),
           max = nan_max(A#state.max, B#state.max),
           sum = A#state.sum + B#state.sum,
           sum2 = A#state.sum2 + B#state.sum2}.


scale(Scale, #state{min = 'NaN', max = 'NaN'} = S) ->
    (scale(S#state{min = 0, max = 0}, Scale))#state{min = 'NaN', max = 'NaN'};

scale(Scale, #state{min = 'NaN'} = S) ->
    (scale(S#state{min = 0}, Scale))#state{min = 'NaN'};

scale(Scale, #state{max = 'NaN'} = S) ->
    (scale(S#state{max = 0}, Scale))#state{max = 'NaN'};

scale(Scale, #state{} = S) ->
    S#state{min = S#state.min * Scale,
            max = S#state.max * Scale,
            sum = S#state.sum * Scale,
            sum2 = S#state.sum2 * (Scale*Scale)}.

ensure_number(<<"NaN">>) -> 'NaN';
ensure_number(I) when is_integer(I) -> I;
ensure_number(F) when is_float(F) -> F.


count(State) ->
    State#state.n.

min(State) ->
    State#state.min.

mean(#state{n = 0}) ->
    'NaN';
mean(State) ->
    State#state.sum / State#state.n.

max(State) ->
    State#state.max.

sum(State) ->
    State#state.sum.

variance(#state { n = N }) when N < 2 ->
    'NaN';
variance(State) ->
    SumSq = State#state.sum * State#state.sum,
    max(0.0, (State#state.sum2 - (SumSq / State#state.n)) / (State#state.n - 1)).


sdev(State) ->
    case variance(State) of
        'NaN' ->
            'NaN';
        Value ->
            math:sqrt(Value)
    end.

summary(State) ->
    {min(State), mean(State), max(State), variance(State), sdev(State)}.


%% ===================================================================
%% Internal functions
%% ===================================================================

nan_min(V1, 'NaN') -> V1;
nan_min('NaN', V1) -> V1;
nan_min(V1, V2)    -> erlang:min(V1, V2).

nan_max(V1, 'NaN') -> V1;
nan_max('NaN', V1) -> V1;
nan_max(V1, V2)    -> erlang:max(V1, V2).


%% ===================================================================
%% Unit Tests
%% ===================================================================

-ifdef(EUNIT).

simple_test() ->
    %% A few hand-checked values
    {1,3.0,5,2.5,1.5811388300841898} = summary(update_all([1,2,3,4,5], new())),
    {1,5.5,10,15.0,3.872983346207417} = summary(update_all(lists:seq(1,10,3), new())).

empty_test() ->
    {'NaN','NaN','NaN','NaN','NaN'} = summary(new()).

update_with_freq_test() ->
    ?assertEqual(update_all([1, 1, 2, 2, 3, 3], new()),
                 update_all_with_freq([{1, 2}, {2, 2}, {3, 2}], new())).

merge_test() ->
    %% Some random data, should be turned into a property test really...
    PointsA = [random:uniform() * 1000 || _ <- lists:seq(1, 1000)],
    PointsB = [random:uniform() * 4711 || _ <- lists:seq(1, 1000)],

    SampleA = lists:foldl(fun update/2, new(), PointsA),
    SampleB = lists:foldl(fun update/2, new(), PointsB),

    Expected = tuple_to_list(summary(lists:foldl(fun update/2, new(),
                                                 PointsA ++ PointsB))),
    Result = tuple_to_list(summary(merge(SampleA, SampleB))),
    ?assert(lists_equal(Expected, Result)),

    ?assertEqual(SampleA, merge(new(), SampleA)),
    ?assertEqual(new(), merge(new(), new())).

scale_test() ->
    Points = [random:uniform() * 1000 || _ <- lists:seq(1, 1000)],
    Alpha = 0.42,
    Scaled = [Alpha * X || X <- Points],
    Sample = lists:foldl(fun update/2, new(), Points),
    Expected = tuple_to_list(summary(lists:foldl(fun update/2, new(), Scaled))),
    Result = tuple_to_list(summary(scale(Alpha, Sample))),
    ?assert(lists_equal(Expected, Result)).

negative_variance_test() ->
    %% Float precision can lead variance becoming (a very small) negative number
    Points = [100, 100, 100],
    Alpha = 1/3,
    Sample = scale(Alpha, lists:foldl(fun update/2, new(), Points)),
    ?assert(variance(Sample) == 0).

lists_equal([], []) ->
    true;
lists_equal([V1 | R1], [V2 | R2]) ->
    case abs(V1-V2) < 0.01 of
        true ->
            lists_equal(R1, R2);
        false ->
            false
    end.

-ifdef(EQC).

prop_main() ->
    ?FORALL(Xlen, choose(2, 100),
        ?LET(Xs, vector(Xlen, int()),
            lists_equal(basho_stats_utils:r_run(Xs,"c(min(x), mean(x), max(x), var(x), sd(x))"),
                tuple_to_list(summary(update_all(Xs, new())))))).

qc_test() ->
    true = eqc:quickcheck(prop_main()).

-endif.


-endif.

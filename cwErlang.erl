-module(cwErlang).
-import(array, [map/2]).
-export([run/0, getResultsForFile/3]).

run() ->
    InputFile = readInput("input.txt"),
    K = list_to_integer(lists:nth(2, InputFile)),
    G = list_to_integer(lists:nth(1, InputFile)),
    DataFiles = lists:nthtail(2, InputFile),

    Main_PID = self(),

    lists:foreach(
        fun(FileName) -> 
            spawn(cwErlang, getResultsForFile, [FileName, G, Main_PID])
        end, DataFiles),

    % There should be as many threads as DataFiles
    Results = waitChildren(length(DataFiles), []),

    Pairs = collectResults(lists:map(fun(Res) -> hd(Res) end, Results), K),
    TotalLines = lists:foldl(fun(X, Acc) -> lists:last(X) + Acc end, 0, Results),

    writeOutput(Pairs, TotalLines).

waitChildren(0, Res) -> Res;
waitChildren(N, Res) ->
    receive   
        Result ->
            AllResults = lists:append(Result, Res),
            waitChildren(N-1, AllResults)
    end.

getResultsForFile(FileName, G, Main_PID) -> 
    link(Main_PID),

    Lines = readInput(FileName),
    LineCount = length(Lines),
    Pairs = getResultsForLines(Lines, G),
    Result = [[Pairs, LineCount]],
    
    Main_PID ! Result.

getResultsForLines(Lines, G) -> flattenListLevel(lists:map(fun(Line) -> getResultsForLine(G, Line) end, Lines)).

getResultsForLine(_, []) -> [];
getResultsForLine(0, _) -> [];
getResultsForLine(G, [X|Xs]) -> lists:append(getResultsForLine(G, Xs), [[X|Y] || Y <- lists:sublist(Xs, G+1)]).

% Couldn't find a way to implement "fromListWith" from erlang, so I split it into to two functions. First grouping, and then counting
% the length of each list. Group is modified version of this https://gist.github.com/jbpotonnier/1310406/25ed800ef6796a998706957a5ef7355f1db0ed4e
group(List) ->
    lists:map(fun({_,Y}) -> Y end,
        dict:to_list(lists:foldr(fun({X,Y}, D) -> dict:append(X, Y, D) end, dict:new(), [ {X, X} || X <- List ]))).

groupsToResults(List) -> lists:map(fun(Xs) -> {lists:nth(1, Xs), length(Xs)} end, List).

collectResults(Pairs, K) ->
    GroupedPairs = group(lists:sort(flattenListLevel(Pairs))),
    Counts = groupsToResults(GroupedPairs),
    Sorted = lists:sort(fun({_, X}, {_, Y}) -> X >= Y end, Counts),
    lists:sublist(Sorted, K).

readInput(InputFile) ->
    {ok, Data} = file:read_file(InputFile),
    string:tokens(erlang:binary_to_list(Data), "\n").

% Had problems with flatten function, thus fold
flattenListLevel(Xs) -> lists:foldl(fun(X, Acc) -> lists:append(Acc, X) end, [], Xs).

writeOutput(Pairs, TotalLines) ->
    lists:foreach(fun(SingleRes) ->
        Pair = element(1, SingleRes),
        file:write_file("output.txt", io_lib:fwrite("~c ~c ~p ~p\n", [hd(Pair), tl(Pair), element(2, SingleRes), TotalLines ]), [append])
    end, Pairs).
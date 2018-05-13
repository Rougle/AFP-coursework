-module(cwErlang).

-import(array, [map/2]).

-export([getResultsForFile/3, getResultsForLines/2, getResultsForLine/2, allGaps/2, allGaps2/3]).


run() ->
    InputFile = readInput("input.txt"),
    K = list_to_integer(lists:nth(2, InputFile)),
    G = list_to_integer(lists:nth(1, InputFile)),
    DataFiles = lists:nthtail(2, InputFile),

    Main_PID = self(),
    ThreadCount = length(DataFiles),

    % create worker threads which process the files
    lists:foreach(
        fun(FileName) -> 
            spawn(cwErlang, getResultsForFile, [FileName, G, Main_PID])
        end, DataFiles),

    % wait here for all the results from the threads
    Results = waitThreads(ThreadCount, []),

    % reduce the results of all threads to one result
    % Gaps = reduceGaps(lists:map( fun(Res) -> hd(Res) end, Results), K),
    % LineCount = lists:foldl( fun(Res, Sum) -> lists:last(Res) + Sum end, 0,Results),

    % write to output file
    %writeOutput(Gaps, LineCount),

    io:fwrite("done").
  
waitThreads(0, Res) -> Res;
waitThreads(N, Res) ->
    receive   
        Result -> % This result is filled from function that counts the gaps
            NewResult = lists:append(Result, Res),
            waitThreads(N-1, NewResult)
    end.


getResultsForFile(FileName, G, Main_PID) -> 
    link(Main_PID),
    Lines = readInput(FileName),
    LineCount = length(Lines),
    Pairs = getResultsForLines(Lines, G),
    Result = [[Pairs, LineCount]],
    
    % send results back to main thread
    Main_PID ! Result.

%ADD FREQ 
getResultsForLines(Lines, G) -> group(lists:foldl(fun(Acc, X) -> lists:append(Acc, X) end, [], lists:map(fun(Line) -> getResultsForLine(G, Line) end, Lines))).

getResultsForLine(_, []) -> [];
getResultsForLine(0, _) -> [];
getResultsForLine(G, [X|Xs]) -> lists:append(getResultsForLine(G, Xs), [[X|Y] || Y <- lists:sublist(Xs, G+1)]).

group(List) ->
    lists:map( fun({_,Y}) -> Y end,
        dict:to_list(lists:foldr(fun({X,Y}, D) -> dict:append(X, Y, D) end, dict:new(), [ {X, X} || X <- List ]))).

% HOW TO ERLANG THIS
%frequency(Xs) -> toList(fromListWith (+) [(X, 1) | X <- Xs]).
% HOW TO ERLANG THIS
%combineResults(Xs) -> toList(fromListWith(+) Xs).

readInput(InputFile) ->
    {ok, Data} = file:read_file(InputFile),
    string:tokens(erlang:binary_to_list(Data), "\n").


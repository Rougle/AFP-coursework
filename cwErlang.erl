-module(cwErlang).

-import(array, [map/2]).

-export([charPairs/0]).


run() ->
    InputFile = readInput("input.txt"),
    K = list_to_integer(lists:nth(2, InputFile)),
    G = list_to_integer(lists:nth(1, InputFile)),
    DataFiles = nthtail(2(InputFile)),

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
            NewResult = lists:append(Result, NewResult),
            waitThreads(N-1, NewResult)
    end.


getResultsForFile(FileName, G, Main_PID -> 
    link(Main_PID)
    Lines <- readInput FileName,
    LineCount = length(Lines),
    Gaps = getResultsForLines(Lines, G),
    Result = [[Gaps, LineCount]],
    
    % send results back to main thread
    Main_PID ! Result.

%GET RID OF MAP HERE
getResultsForLines(Lines, G) -> foldListOfLists(lookupPairsFromLines(Lines, G))

lookupPairsFromLines(Lines, G -> List.map (fun(Line) -> lookupPairs(Line, G) Lines

lookupPairs(Line, G) -> List.map (fun({X,Y} -> (X:Y:[]), lookupPair(Line (X,Y) G) end) charPairs() 


% WRITE ELEMINDECES IN ERLANG? FIND OTHER SOLUTION?
lookupPair(InputLine, {X,Y},G -> 
    XIndeces = findCharIndeces(InputLine, X),
    YIndeces = findCharIndeces(InputLine, Y),
    getPairCount(XIndeces, YIndeces, G)



% This might not work in erlang. Probably wont. Fuck.
charPairs() -> [{I, J} || I <- [$a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z], J <- [$a, $b, $c, $d, $e, $f, $g, $h, $i, $j, $k, $l, $m, $n, $o, $p, $q, $r, $s, $t, $u, $v, $w, $x, $y, $z] ].

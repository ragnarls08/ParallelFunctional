-module(sudoku).
%-include_lib("eqc/include/eqc.hrl").
-import(ptools,[master/3]).
-compile(export_all).

%% %% generators

%% matrix(M,N) ->
%%     vector(M,vector(N,nat())).

%% matrix transpose

transpose([Row]) ->
    [[X] || X <- Row];
transpose([Row|M]) ->
    [[X|Xs] || {X,Xs} <- lists:zip(Row,transpose(M))].

%% prop_transpose() ->
%%     ?FORALL({M,N},{nat(),nat()},
%% 	    ?FORALL(Mat,matrix(M+1,N+1),
%% 		    transpose(transpose(Mat)) == Mat)).

%% map a matrix to a list of 3x3 blocks, each represented by the list
%% of elements in row order

triples([A,B,C|D]) ->
    [[A,B,C]|triples(D)];
triples([]) ->
    [].

blocks(M) ->
    Blocks = [triples(X) || X <- transpose([triples(Row) || Row <- M])],
    lists:append(
      lists:map(fun(X)->
			lists:map(fun lists:append/1, X)
		end,
		Blocks)).

unblocks(M) ->
    lists:map(
      fun lists:append/1,
      transpose(
	lists:map(
	  fun lists:append/1,
	  lists:map(
	    fun(X)->lists:map(fun triples/1,X) end,
	    triples(M))))).

%% prop_blocks() ->
%%     ?FORALL(M,matrix(9,9),
%% 	    unblocks(blocks(M)) == M).

%% decide whether a position is safe

entries(Row) ->
    [X || X <- Row,
	  1 =< X andalso X =< 9].

safe_entries(Row) ->
    Entries = entries(Row),
    lists:sort(Entries) == lists:usort(Entries).

safe_rows(M) ->
    lists:all(fun safe_entries/1,M).

safe(M) ->
    safe_rows(M) andalso
	safe_rows(transpose(M)) andalso
	safe_rows(blocks(M)).

%% fill blank entries with a list of all possible values 1..9

fill(M) ->
    Nine = lists:seq(1,9),
    [[if 1=<X, X=<9 ->
	      X;
	 true ->
	      Nine
      end
      || X <- Row]
     || Row <- M].

%% refine entries which are lists by removing numbers they are known
%% not to be

refine(M) ->
    NewM =
	refine_rows(
	  transpose(
	    refine_rows(
	      transpose(
		unblocks(
		  refine_rows(
		    blocks(M))))))),
    if M==NewM ->
	    M;
       true ->
	    refine(NewM)
    end.

refine_rows(M) ->
    [begin
	 Entries = entries(Row),
	 [if is_list(X) ->
		  case X--Entries of
		      [] ->
			  exit(no_solution);
		      [Y] ->
			  Y;
		      NewX ->
			  NewX
		  end;
	     true ->
		  X
	  end
	  || X <- Row]
     end
     || Row <- M].

is_exit({'EXIT',_}) ->
    true;
is_exit(_) ->
    false.

%% is a puzzle solved?

solved(M) ->
    lists:all(fun solved_row/1,M).

solved_row(Row) ->
    lists:all(fun(X)-> 1=<X andalso X=<9 end, Row).

%% how hard is the puzzle?

hard(M) ->		      
    lists:sum(
      [lists:sum(
	 [if is_list(X) ->
		  length(X);
	     true ->
		  0
	  end
	  || X <- Row])
       || Row <- M]).

%% choose a position {I,J,Guesses} to guess an element, with the
%% fewest possible choices

guess(M) ->
    Nine = lists:seq(1,9),
    {_,I,J,X} =
	lists:min([{length(X),I,J,X}
		   || {I,Row} <- lists:zip(Nine,M),
		      {J,X} <- lists:zip(Nine,Row),
		      is_list(X)]),
    {I,J,X}.

%% given a matrix, guess an element to form a list of possible
%% extended matrices, easiest problem first.

guesses(M) ->
    {I,J,Guesses} = guess(M),
    Ms = [catch refine(update_element(M,I,J,G)) || G <- Guesses],
    SortedGuesses =
	lists:sort(
	  [{hard(NewM),NewM}
	   || NewM <- Ms,
	      not is_exit(NewM)]),
    [G || {_,G} <- SortedGuesses].

update_element(M,I,J,G) ->
    update_nth(I,update_nth(J,G,lists:nth(I,M)),M).

update_nth(I,X,Xs) ->
    {Pre,[_|Post]} = lists:split(I-1,Xs),
    Pre++[X|Post].

%% prop_update() ->
%%     ?FORALL(L,list(int()),
%% 	    ?IMPLIES(L/=[],
%% 		     ?FORALL(I,choose(1,length(L)),
%% 			     update_nth(I,lists:nth(I,L),L) == L))).

%% solve a puzzle

solve(M) ->
    solve_refined(refine(fill(M))).

solve_refined(M) ->
    case solved(M) of
	true ->
	    M;
	false ->
	    solve_one(guesses(M))
    end.

solve_one([]) ->
    exit(no_solution);
solve_one([M]) ->
    solve_refined(M);
solve_one([M|Ms]) ->
    case catch solve_refined(M) of
	{'EXIT',no_solution} ->
	    solve_one(Ms);
	Solution ->
	    Solution
    end.

%% benchmarks

-define(EXECUTIONS,1000).

bm(F) ->
    {T,_} = timer:tc(?MODULE,repeat,[F]),
    T/?EXECUTIONS/1000.

repeat(F) ->
    [F() || _ <- lists:seq(1,?EXECUTIONS)].

benchmarks(Puzzles) ->
    [{Name,bm(fun()->solve(M) end)} || {Name,M} <- Puzzles].

benchmarks() ->
  {ok,Puzzles} = file:consult("problems.txt"),
  timer:tc(?MODULE,benchmarks,[Puzzles]).

%% Parallel version

%Each batch of puzzles is evaluated on its own thread
%up to Num_Workers
benchmarksP(Num_Workers) ->
  {ok,Puzzles} = file:consult("problems.txt"),
  timer:tc(?MODULE,benchmarksP,[[Puzzles],Num_Workers]).

benchmarksP([Puzzles], Num_Workers) ->
	master(Puzzles, Num_Workers, {?MODULE, bench}).

bench({Name,M}) ->
	{Name, bm(fun()->solve(M) end)}.
	

% might need to make a loop to really measure the overall gain of time





% %% Old Parallel version

% %Each batch of puzzles is evaluated on its own thread
% %up to Num_Workers
% benchmarksP(Num_Workers) ->
%   {ok,Puzzles} = file:consult("problems.txt"),
%   timer:tc(?MODULE,benchmarksP,[[Puzzles],Num_Workers]).

% benchmarksP([Puzzles], Num_Workers) ->
% 	master(Puzzles, Num_Workers).

% %Wrapper for init values of master
% master(P, N) ->
% 	master(P, [], N, 0).

% %Recolting last workers once job done
% master([],R,N,A) ->
%  	if 	A=:=0 ->
%  			R;
%  		true ->
% 		 	receive
%  		 		{Name,Bm} ->
%  		 			master([],[{Name,Bm}|R],N,A-1)
%  		 	end
%  	end;
% %Waiting for workers te be free
% master(P,R,0,A) -> 
%  	receive
%  		{Name,Bm} ->
%  			master(P, [{Name,Bm}|R], 1, A-1)
%  	end;
% %Sending worker to his job
% master([H|T],R,N,A) ->
% 	spawn(sudoku, worker,[self(),H]),
% 	master(T, R, N-1,A+1).

% %Worker does his job
% worker(Master_PID, {Name,M}) ->
% 	Master_PID ! {Name, bm(fun()->solve(M) end)}.


% 2> sudoku:benchmarks().
% {28011247,
%  [{wildcat,0.175258},
%   {diabolical,1.765024},
%   {vegard_hanssen,5.669718},
%   {challenge,2.3675770000000003},
%   {challenge1,10.573148},
%   {extreme,5.049308},
%   {seventeen,2.411161}]}
% 3> sudoku:benchmarksP(1).
% {29557304,
%  [{seventeen,2.558498},
%   {extreme,5.345056},
%   {challenge1,11.092886},
%   {challenge,2.533143},
%   {vegard_hanssen,5.996325},
%   {diabolical,1.847451},
%   {wildcat,0.183077}]}
% 4> sudoku:benchmarksP(2).
% {15461774,
%  [{challenge1,11.068845999999999},
%   {seventeen,2.6306599999999998},
%   {extreme,5.511158},
%   {vegard_hanssen,6.105086},
%   {challenge,2.5147890000000004},
%   {diabolical,1.877693},
%   {wildcat,0.183351}]}
% 5> sudoku:benchmarksP(3).
% {13330488,
%  [{challenge1,11.305231},
%   {seventeen,2.6890639999999997},
%   {extreme,5.642435000000001},
%   {vegard_hanssen,6.437106},
%   {challenge,2.660611},
%   {diabolical,2.024762},
%   {wildcat,0.170151}]}
% 6> sudoku:benchmarksP(4).
% {11402301,
%  [{challenge1,11.21719},
%   {extreme,5.5740479999999994},
%   {vegard_hanssen,6.517399},
%   {seventeen,2.763668},
%   {challenge,2.72596},
%   {diabolical,2.040557},
%   {wildcat,0.184983}]}
% 7> sudoku:benchmarksP(5).
% {13334981,
%  [{challenge1,13.332244000000001},
%   {vegard_hanssen,6.479533},
%   {extreme,5.685018},
%   {challenge,5.145734999999999},
%   {seventeen,2.7207510000000004},
%   {diabolical,2.028387},
%   {wildcat,0.19488999999999998}]}
% 8> sudoku:benchmarksP(6).
% {12787206,
%  [{challenge1,12.785253},
%   {vegard_hanssen,8.391698},
%   {extreme,6.114833},
%   {challenge,4.3892809999999995},
%   {diabolical,4.110467},
%   {seventeen,2.769498},
%   {wildcat,0.203826}]}
% 9> sudoku:benchmarksP(7).
% {13271951,
%  [{challenge1,13.270314},
%   {vegard_hanssen,7.623057},
%   {extreme,5.737845},
%   {diabolical,4.371768},
%   {seventeen,4.312239},
%   {challenge,2.829621},
%   {wildcat,0.5848949999999999}]}
% 10> sudoku:benchmarksP(8).
% {12774814,
%  [{challenge1,12.772287},
%   {extreme,7.257149},
%   {vegard_hanssen,6.608787},
%   {challenge,4.830015},
%   {diabolical,4.322634},
%   {seventeen,3.242264},
%   {wildcat,0.502608}]}

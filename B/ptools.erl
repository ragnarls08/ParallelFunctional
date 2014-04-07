-module(ptools).
-compile(export_all).

% master takes a list of tasks P, a number of workers N and a function F to apply
% example of use : 
% master([1,2,3], 2,  {?MODULE, functionName}).
master(P, N, F) ->
	master(P, [], N, F, 0).

%Recolting last workers once job done
master([],R,N,F,A) ->
 	if 	A=:=0 ->
 			R;
 		true ->
		 	receive
 		 		Msg ->
 		 			master([],[Msg|R],N,F,A-1)
 		 	end
 	end;
%Waiting for workers te be free
master(P,R,0,F,A) -> 
 	receive
 		Msg ->
 			master(P, [Msg|R], 1, F, A-1)
 	end;
%Sending a worker to his job
master([H|T],R,N,F,A) ->
	spawn(ptools, worker,[self(),H,F]),
	master(T, R, N-1,F,A+1).

%Worker does his job and send results to master
worker(Master_PID, H, F) ->
	Master_PID ! F(H).

test() ->
	%master([1,2,3], 2, fun() -> square end).
	master([1,2,3], 2, {?MODULE, square}).
	
square(I) -> 
	I*I.
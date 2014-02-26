-module(wordlist).
-author('schaturaprue@cs.hmc.edu').
-export([load/1, distance/3, correct/2, suggestions/2, main/1, add/2, remove/2]).

% It is the maximum Levenshtein distance to determine whether two words
% are still considered "close".
-define(MAX_ERROR_DISTANCE, 1).

% load/1
% The parameter to this function is the path to a dictionary file.
% The function returns a list containing the words in the dictionary file.
% if anything goes wrong opening the file or reading strings from it,
% the resulting list should be empty.
% (adapted from boilerplate code from
% http://www.erlang.org/upload/klacke_examples/count_chars.erl)
load(Pathname) ->
    {Status, Data} = file:read_file(Pathname),
	if 	Status == ok ->
			[erlang:binary_to_list(X) || X <-
			  binary:split(Data, [<<"\n">>, <<"\r">>], [global]),
			  erlang:length(binary_to_list(X)) > 0];
		true ->
			[]
	end.

% distance/3
% The parameters are a word X, a word Y, and an integer threshold T. 
% The function returns the distance between x and y up to threshold t;
% if the distance is greater than t,
% the function may return any integer greater than t.
distance(A, Y, T) ->
	lev(A, Y, erlang:length(A), erlang:length(Y), T).

% f/2
% A helper method for the lev method; f(Character_1, Character_2) = 0 if
% Character_1 is equivalent to Character_2 and 1 otherwise.
f(Character_1, Character_2) ->
	if Character_1 == Character_2 ->
			0;
		true ->
			1
	end.

% lev/5
% A helper method that corresponds to the lev opeartor in the assignment.
% And note that Erlang starts counting from 1 rather than 0.
lev(W1, W2, I, J, T) ->
	if 	T < 0 -> 0;
		I == 0; J == 0 -> erlang:max(I, J);
		true -> erlang:min(erlang:min(lev(W1, W2, I - 1, J, T - 1) + 1,
						lev(W1, W2, I, J - 1, T - 1) + 1),
					lev(W1, W2, I - 1, J - 1, T) +
				    f(lists:nth(I, W1), lists:nth(J, W2))
				)
	end.

% correct/2
% The parameters are a word W and a list of words L.
% The function returns true if W appears in L and false otherwise.
correct(W, L) ->
	lists:member(W, L).

% add/2
% The parameters are a word W and a list of words L.
% The function adds W to L to W is not in L, else return L.
add(W, L) ->
    AlreadyIn = lists:member(W, L),
	if AlreadyIn ->
		 L;
	   true ->
		 W ++ [L]
	end.

% remove/2
% The parameters are a word W and a list of words L.
% The function removes W to L to W is in L, else return L.
remove(W, L) ->
    W -- [L].

% suggestions/2
% The parameters are a word W and a list of words L. 
% The function returns a list containing all the suggested spellings for W
% that appear in L in alphabetical order. 
% If there are no suggested spellings for W, an empty list should be returned.
suggestions(W, L) ->
	lists:sort([Word || Word <- L,
		distance(W, Word, ?MAX_ERROR_DISTANCE + 1) =< ?MAX_ERROR_DISTANCE]).

% pretty_print_list/1
% The parameter is a list L. If L = [1, 2, 3], it returns "[1, 2, 3]".
pretty_print_list(L) ->
    "[" ++ string:join(L, ", ") ++ "]".

% io_loop/1
% The input/output loop.
io_loop(Dictionary) ->
	Raw = io:get_line("Enter a word > "),
	if 	Raw == eof ->
			erlang:halt();
		true ->
			Word = string:strip(string:strip(Raw, right, 10), right, 13),
			io:format("Correct: "),
			io:format(correct(Word, Dictionary)),
			io:format("~n"),
			io:format("Suggestions: "),
			io:format(pretty_print_list(suggestions(Word, Dictionary))),
			io:format("~n"),
			io:format("~n"),
			io_loop(Dictionary)
	end.

% A "main" function for interactive usage of the program.
% The parameter is a list whose only element is the pathname
% of a dictionary file.
main([Pathname]) ->
	Dictionary = load(Pathname),
	io_loop(Dictionary).
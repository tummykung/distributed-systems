%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Erlang Messaging Server
%% @author Tum Chaturapruek
%% @doc Receives a message (with return address and ref) from an Erlang
%% process and sends a reply of word checking and suggestions.

-module(spellingserver).
-import(wordlist, [load/1, correct/2, suggestions/2, add/2, remove/2]).

%% ====================================================================
%% Public API
%% ====================================================================

-export([main/1]).

% The main function, as required by the assignment.
main(Params) ->
	try
		RegisteredName = hd(Params), % the first parameter is node name
		DistionaryPath = hd(tl(Params)), % the second parameter is a dictionary path
		Dictionary = wordlist:load(DistionaryPath),
        %% IMPORTANT: Start the empd daemon!
		_ = os:cmd("epmd -daemon"),
		{_, _, Micro} = os:timestamp(),
		net_kernel:start([list_to_atom("schaturaprue" ++ integer_to_list(Micro)), shortnames]),
		register(list_to_atom(RegisteredName), self()),
		print("Ready to receive messages. Registered as ~p at node ~p, waiting for messages.~n",
				  [RegisteredName, node()]),
		handle_message({}, Dictionary)
	catch
		_:_ -> print("Error parsing command line parameters or dictionary..~n")
	end,
	halt().

%% ====================================================================
%% Internal functions
%% ====================================================================

% Handles a message using the specified reply
handle_message(Reply, Dictionary) ->
	receive
	    {Client, Ref, check, Word} ->
		  print("Received check query from ~p for " ++ Word ++ "~n", [Client]),
	      case wordlist:correct(Word, Dictionary) of
		    true ->
		      print("  Word is spelled correctly.~n"),
		      To_Send = {Ref, correct};
		    false ->
	          Suggestions = wordlist:suggestions(Word, Dictionary),
			  print("  " ++ Word ++ " is spelled incorrectly. Suggestions: ~p~n", [Suggestions]),
		      To_Send = {Ref, incorrect, Suggestions}
	      end,
	      Client ! To_Send,
		  NewDictionary = Dictionary;
	    {Client, Ref, add, Word} ->
		  print("Received add query from ~p for " ++ Word ++ "~n", [Client]),
		  NewDictionary = wordlist:add(Word, Dictionary),
		  Client ! {Ref, add};
	  	{Client, Ref, remove, Word} ->
		  print("Received remove query from ~p for " ++ Word ++ "~n", [Client]),
		  NewDictionary = wordlist:remove(Word, Dictionary),
		  Client ! {Ref, remove};
		Message -> 
			print("Unexpected message received: ~p", [Message]),
			NewDictionary = Dictionary
	end,
	handle_message(Reply, NewDictionary).


% Helper functions for timestamp handling.
get_two_digit_list(Number) ->
	if Number < 10 ->
		   ["0"] ++ integer_to_list(Number);
	   true ->
		   integer_to_list(Number)
	end.

get_three_digit_list(Number) ->
	if Number < 10 ->
		   ["00"] ++ integer_to_list(Number);
	   Number < 100 ->
	       ["0"] ++ integer_to_list(Number);
	   true ->
		   integer_to_list(Number)
	end.

get_formatted_time() ->
	{MegaSecs, Secs, MicroSecs} = now(),
	{{Year, Month, Date},{Hour, Minute, Second}} =
		calendar:now_to_local_time({MegaSecs, Secs, MicroSecs}),
	integer_to_list(Year) ++ ["-"] ++
	get_two_digit_list(Month) ++ ["-"] ++
	get_two_digit_list(Date) ++ [" "] ++
	get_two_digit_list(Hour) ++ [":"] ++
	get_two_digit_list(Minute) ++ [":"] ++
	get_two_digit_list(Second) ++ ["."] ++
	get_three_digit_list(MicroSecs div 1000).

% print/1
% includes system time.
print(To_Print) ->
	io:format(get_formatted_time() ++ ": " ++ To_Print).

% print/2
print(To_Print, Options) ->
	io:format(get_formatted_time() ++ ": " ++ To_Print, Options).
	
%% CSCI182E - Distributed Systems
%% Harvey Mudd College
%% Erlang Messaging Client
%% @author Tum Chaturapruek
%% @doc Sends a message to a specified Erlang nodename and spell checks words.

-module(spellingclient).

-define(TIMEOUT, 10000).

%% ====================================================================
%% Public API
%% ====================================================================

-export([main/1]).


send_message(Ref, Node, Messages) ->
  if Messages == [] ->
	   print("All queries complete.~n"),
	   true;
	 true -> 
	  Message = hd(Messages),
	  TheRest = tl(Messages),
	  print("Querying Server (" ++ Message ++ ")~n"),
	  {spelling, Node} ! {self(), Ref, check, Message},
	  receive
		{Ref, correct} ->
		  print(Message ++ " is spelled correctly.~n");
		{Ref, incorrect, Suggestions} ->
		  	if Suggestions == [] ->
				 print(Message ++ " is spelled incorrectly. No suggestions.~n");
			   true ->
				 print(Message ++ " is spelled incorrectly. Suggestions: ~p~n", [Suggestions])
			   end;
		{Ref, add} ->
		  print(Message ++ " was sucessfully added.~n");
		{Ref, remove} ->
		  print(Message ++ " was sucessfully removed.~n");
		Reply ->
		  print("Got unexpected message: ~p~n", [Reply])
	  after ?TIMEOUT -> print("Timed out waiting for reply!")
	  end,
	  send_message(Ref, Node, TheRest)
	end.

% The main function, as required by the assignment.
main(Params) ->
	try 
		% the first parameter is destination node name
		NodeName = list_to_atom(hd(Params)),
		% subsequent parameters are the message to send
		Messages = tl(Params),
        %% IMPORTANT: Start the empd daemon!
		_ = os:cmd("epmd -daemon"),
		% format microseconds of timestamp to get an 
		% effectively-unique node name
		{_, _, Micro} = os:timestamp(),
        net_kernel:start([list_to_atom("schaturaprue" ++ integer_to_list(Micro)), 
        				  shortnames]),
		register(spelling, self()),
		Ref = make_ref(), % make a ref so I know I got a valid response back
		send_message(Ref, NodeName, Messages)
	catch
		_:_ -> print("Error parsing command line parameters or resolving node name.~n")
	end,
	halt().

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
	
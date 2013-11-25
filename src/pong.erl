-module(pong).

-export([start_client/1,start_server/0,stop_server/0]).

-export([]).

start_client(Server_Address) ->
    contact_server(Server_Address),
    Pid = spawn(?MODULE, client_loop, [Server_Address, unknown]),
    init_input_listener(Pid).
    

client_loop(Server_Address, Key) ->
    receive
	request -> Server_Adress ! {self(), Key},
		   client_loop(Server_Adress, unknown);
	{frame, {Ball_Pos,
		 {self(), My_Pos},
		 {_, Foe_Pos}}} -> draw_frame(Ball_Pos, My_Pos, Foe_Pos),
				   client_loop(Server_Address, Key);
	{input, '\ESC'} -> ok;
	{input, Key} -> client_loop(Server_Address, Key)
    end.


drawframe({XB,YB}, {X1,Y1}, {X2,Y2}) ->
    .



start_server() ->
    cfg = read_config(),
    register(serverpong, spawn(?MODULE, init_server, [cfg])).
    


stop_server() ->
    serverpong ! stop.



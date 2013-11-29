-module(pong).

-export([start_client/1,start_server/0,stop_server/0]).

-export([client_loop/1, init_server/1]).

start_client(Server_Node) ->
    contact_server(Server_Node),
    Pid = spawn(?MODULE, client_loop, [Server_Node, unknown]),
    init_input_listener(Pid).
    

contact_server(Server_Node)->
    {pong_server, Server_Node} ! {register_player, self()},
    monitor_node(Server_Node, true),
    receive
	gamestart -> ok
    after 
	60000 ->
	    exit(timeout)
    end.


client_loop(Server_Node, Key) ->
    receive
	request            -> {pong_server, Server_Node} ! {self(), Key},
			      client_loop(Server_Node, unknown);
	{frame, {gamestate, Field_Size,
		 {ball, Ball_Pos},
		 {bot_player, My_Pos},
		 {top_player, Foe_Pos}}} -> draw_frame(Field_Size,
					 Ball_Pos,
					 My_Pos,
					 Foe_Pos),
			      client_loop(Server_Node, Key);
	{nodedown, Node}   -> {error, nodedown};
	{input, '\ESC'}    -> ok;
	{input, Key}       -> client_loop(Server_Address, Key)
    end.



draw_frame({XG,YG}, {XB,YB}, X1, X2) ->
    Str_Frame = pong_str({XG,YG}, {XB,YB}, X1, X2),
    io:format("%p", [Str_Frame]).

pong_str({XG,YG}, {XB,YB}, X1, X2) ->
    V_line_temp = string:chars($_,XG," ~n"),
    V_line = string:chars($ ,1,V_line_temp),
    H_line = pong_str_aux({XG,YG}, {XB,YB}, X1, X2, YG, ""),
    Temp = string:concat(V_line, H_line),
    string:concat(Temp,V_line).

pong_str_aux(_, _, _, _, 0, Acc_Str) ->
    Acc_Str;
pong_str_aux({XG,YG}, {XB, 1}, X1, X2, 1, Acc_Str) ->
    Temp_Str = line_str(XG, XB, X2),
    pong_str_aux({XG,YG}, {XB, 1}, X1, X2, 0, concat(Temp_Str,Acc_Str));
pong_str_aux({XG,YG}, {XB, YG}, X1, X2, YG, Acc_Str) ->
    Temp_Str = line_str(XG, XB, X1),
    pong_str_aux({XG,YG}, {XB, YG}, X1, X2, YG-1, concat(Temp_Str,Acc_Str));
pong_str_aux({XG,YG}, Ball, X1, X2, YG, Acc_Str) ->
    Temp_Str = line_str(XG, none, X1),
    pong_str_aux({XG,YG}, Ball, X1, X2, YG-1, concat(Temp_Str,Acc_Str));
pong_str_aux({XG,YG}, Ball, X1, X2, 1, Acc_Str) ->
    Temp_Str = line_str(XG, none, X2),
    pong_str_aux({XG,YG}, Ball, X1, X2, 0, concat(Temp_Str,Acc_Str));
pong_str_aux({XG,YG}, {XB,YC}, X1, X2, YC, Acc_Str) ->
    Temp_Str = line_str(XG, XB, none),
    pong_str_aux({XG,YG}, {XB,YC}, X1, X2, YC-1, concat(Temp_Str,Acc_Str));
pong_str_aux({XG,YG}, Ball, X1, X2, YC, Acc_Str) ->
    Temp_Str = line_str(XG, none, none),
    pong_str_aux({XG,YG}, Ball, X1, X2, YC-1, concat(Temp_Str,Acc_Str));
    

line_str(XG, none, none) ->
    Temp = string:chars($ , XG,"|~n"),
    string:chars($|, 1, Temp);
line_str(XG, XB, none) ->
    Temp1 = string:chars($ , XG-1-XB,"|~n"),
    Temp2 = string:chars($O, 1, Temp1),
    Temp3 = string:chars($ , XB, Temp2),
    string:chars($|, 1, Temp3);
line_str(XG, none, X) ->
    Temp1 = string:chars($ , XG-1-(X+2),"|~n"),
    Temp2 = string:chars($H, 5, Temp1),
    Temp3 = string:chars($ , X-2, Temp2),
    string:chars($|, 1, Temp3);
line_str(XG, XB, X) when X > XB->
    Temp1 = string:chars($ , XG-1-(X+2),"|~n"),
    Temp2 = string:chars($H, 5, Temp1),
    Temp3 = string:chars($ , X-2-XB+1, Temp2),
    Temp4 = string:chars($O, 1, Temp3),
    Temp5 = string:chars($ , XB, Temp4),
    string:chars($|, 1, Temp5);
line_str(XG, XB, X) ->
    Temp1 = string:chars($ , XG-1-XB,"|~n"),
    Temp2 = string:chars($O, 1, Temp1),
    Temp3 = string:chars($ , XB-(X+2)-1, Temp2),
    Temp4 = string:chars($H, 5, Temp3),
    Temp5 = string:chars($ , XB, Temp4),
    string:chars($|, 1, Temp5).
    
    

%% TODO
init_input_listener(Pid)->
    .

start_server(Config_File) ->
    register(serverpong, spawn(?MODULE, init_server, [Config_File])).


init_server(Config_File)->
    GameState = read_config(Config_File),
    {Pid1, Pid2} = wait_players(),
    game_loop(GameState, Pid1, Pid2).



read_config(Config_File) ->
    {ok, File_Device} = file:open(Config_File, read),
    {ok, [Gx, Gy]} = fread(File_Device, '', "%d %d"),
    {gamestate,
     {Gx, Gy},
     {ball, {B1x, B2y}},
     {bot_player, {P1x, P1y}},
     {top_player, {P2x, P2y}}}.    


wait_players() ->
    Pid1 = player_connection(),
    Pid2 = player_connection(),
    Pid1 ! gamestart,
    Pid2 ! gamestart,
    {Pid1, Pid2}.


player_connection() ->
    receive
	{register_player, Pid} -> Pid
    after
	60000 ->
	    exit(timeout)
    end.


game_loop(GameState, Pid1, Pid2) ->
    Pid1 ! request,
    Pid2 ! request,
    {Key1, Key2} = receive_inputs(none, none),
    NGameState = update_GameState(GameState, Key1, Key2),
    Pid1 ! {frame, NGameState},
    Pid2 ! {frame, flip_GameState(NGameState)},
    timer:sleep(500),
    game_loop(NGameState, Pid1, Pid2).

%%TODO
receive_inputs(none, none) ->
    .

%%TODO
update_GameState(GameState, Key1, Key2) ->
    .

%%TODO
flip_GameState(NGameState) ->
    .


stop_server() ->
    serverpong ! stop.



-module(pong_client).

-export([start_client/1]).

-export([client_loop/2]).

start_client(Server_Node) ->
    contact_server(Server_Node),
    client_loop(Server_Node, unknown).
    

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
	request          -> {pong_server, Server_Node} ! {self(), Key},
			      client_loop(Server_Node, unknown);
	{frame, quit}    -> ok;
	{frame, {gamestate, Field_Size,
		 {ball, Ball_Pos, _},
		 {bot_player, {My_Pos, _}},
		 {top_player, {Foe_Pos, _}}}} -> draw_frame(Field_Size,
							    Ball_Pos,
							    My_Pos,
							    Foe_Pos),
						 client_loop(Server_Node, Key);
	{nodedown, _}   -> {error, nodedown};

	{input, quit}   -> ok;
	{input, Key}    -> client_loop(Server_Node, Key)
    end.



draw_frame({XG,YG}, {XB,YB}, X1, X2) ->
    Str_Frame = pong_str({XG,YG}, {XB,YB}, X1, X2),
    io:format(Str_Frame).

pong_str({XG,YG}, {XB,YB}, X1, X2) ->
    V_line_temp = string:chars($_,XG," ~n"),
    V_line = string:chars($ ,1,V_line_temp),
    H_line = pong_str_aux({XG,YG}, {XB,YB}, X1, X2, YG-1, ""),
    Temp = string:concat(V_line, H_line),
    string:concat(Temp,V_line).

pong_str_aux(_, _, _, _, -1, Acc_Str) ->
    Acc_Str;
pong_str_aux({XG,YG}, {XB, 0}, X1, X2, 0, Acc_Str) ->
    Temp_Str = line_str(XG, XB, X2),
    pong_str_aux({XG,YG}, {XB, 0}, X1, X2, -1,
		 string:concat(Temp_Str,Acc_Str));
pong_str_aux({XG,YG}, {XB, YC}, X1, X2, YC, Acc_Str)
  when YG-1 == YC ->
    Temp_Str = line_str(XG, XB, X1),
    pong_str_aux({XG,YG}, {XB, YC}, X1, X2, YC-1,
		 string:concat(Temp_Str,Acc_Str));
pong_str_aux({XG,YG}, Ball, X1, X2, 0, Acc_Str) ->
    Temp_Str = line_str(XG, none, X2),
    pong_str_aux({XG,YG}, Ball, X1, X2, -1,
		 string:concat(Temp_Str,Acc_Str));
pong_str_aux({XG,YG}, Ball, X1, X2, YC, Acc_Str)
  when YG-1 == YC ->
    Temp_Str = line_str(XG, none, X1),
    pong_str_aux({XG,YG}, Ball, X1, X2, YC-1,
		 string:concat(Temp_Str,Acc_Str));
pong_str_aux({XG,YG}, {XB,YC}, X1, X2, YC, Acc_Str) ->
    Temp_Str = line_str(XG, XB, none),
    pong_str_aux({XG,YG}, {XB,YC}, X1, X2, YC-1,
		 string:concat(Temp_Str,Acc_Str));
pong_str_aux({XG,YG}, Ball, X1, X2, YC, Acc_Str) ->
    Temp_Str = line_str(XG, none, none),
    pong_str_aux({XG,YG}, Ball, X1, X2, YC-1,
		 string:concat(Temp_Str,Acc_Str)).
    

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
line_str(XG, XB, X) when X > XB ->
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

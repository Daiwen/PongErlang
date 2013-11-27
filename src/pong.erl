-module(pong).

-export([start_client/1,start_server/0,stop_server/0]).

-export([]).

start_client(Server_Address) ->
    contact_server(Server_Address),
    Pid = spawn(?MODULE, client_loop, [Server_Address, unknown]),
    init_input_listener(Pid).
    

%% TODO
contact_server(Server_Address)->
    .


client_loop(Server_Address, Key) ->
    receive
	request -> Server_Adress ! {self(), Key},
		   client_loop(Server_Adress, unknown);
	{frame, {{Field_Size},
		 Ball_Pos,
		 My_Pos,
		 Foe_Pos}} -> draw_frame(Field_Size,
					      Ball_Pos,
					      My_Pos,
					      Foe_Pos),
				   client_loop(Server_Address, Key);

	{input, '\ESC'} -> ok;
	{input, Key} -> client_loop(Server_Address, Key)
    end.


%% TODO
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

start_server() ->
    cfg = read_config(),
    register(serverpong, spawn(?MODULE, init_server, [cfg])).

%%TODO    
init_server(cfg)->
    .

stop_server() ->
    serverpong ! stop.



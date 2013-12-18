-module(pong_client).
-behaviour(gen_server).

-export([start/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).


%%Gen server
init([Server_Node]) ->
    contact_server(Server_Node).


contact_server(Server_Node)->
    {pong_server, Server_Node} ! {register_player, self()},
    io:format("Registering to the pong server ~n"),
    monitor_node(Server_Node, true),
    receive
	gamestart -> {ok, {Server_Node, unknown}}
    after 
	60000     -> {stop, timeout}
    end.




handle_call(request, _From, {Server_Node, Key}) ->
    {reply, {self(), Key}, {Server_Node, unknown}};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({frame, {gamestate, Field_Size,
		     {ball, Ball_Pos, _Ball_Dir},
		     {bot_player, {My_Posx , _My_Posy}},
		     {top_player, {Foe_Posx, _Foe_Posy}}}}, State) ->
    draw_frame(Field_Size, Ball_Pos, My_Posx, Foe_Posx),
    {noreply, State};
handle_cast({input, stop}, {Server_Node, Key}) ->
    Server_Node ! {self(), stop},
    {stop, normal, {Server_Node, Key}};
handle_cast({input, Key}, {Server_Node, _Old_Key}) ->
    {noreply, {Server_Node, Key}};
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({nodedown, _}, State) -> 
    {stop, nodedown, State};
handle_info(Msg, State) -> 
    io:format("Unknown message: ~p~n", [Msg]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


%%%%



start(Server_Node) ->
    gen_server:start(?MODULE, [Server_Node], []).
    





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

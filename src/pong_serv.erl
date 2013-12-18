-module(pong_serv).

-export([start_server/1,stop_server/0]).

-export([init_server/1]).


start_server(Config_File) ->
    register(pong_server, spawn(?MODULE, init_server, [Config_File])).


init_server(Config_File)->
    GameState = read_config(Config_File),
    {Pid1, Pid2} = wait_players(),
    Pid1 ! {frame, GameState},
    Pid2 ! {frame, flip_GameState(GameState)},
    game_loop(GameState, Pid1, Pid2).



read_config(Config_File) ->
    {ok, File_Device} = file:open(Config_File, read),
    {ok, [Gx, Gy]} = io:fread(File_Device, '', "~d ~d"),
    {gamestate,
     {Gx, Gy},
     {ball, {(Gx div 2)+1, Gy div 2}, {0, 1}},
     {bot_player, {Gx div 2, Gy-1}},
     {top_player, {Gx div 2, 0}}}.    


wait_players() ->
    Pid1 = player_connection(),
    Pid2 = player_connection(),
    Pid1 ! gamestart,
    Pid2 ! gamestart,
    {Pid1, Pid2}.


player_connection() ->
    receive
	{register_player, Pid} ->  io:format("Received a registration request~n"),
				   Pid
    after
	60000 ->
	    exit(timeout)
    end.


game_loop(GameState, Pid1, Pid2) ->
    Pid1 ! request,
    Pid2 ! request,
    {Key1, Key2} = receive_inputs(Pid1, Pid2, none, none),
    NGameState = update_GameState(GameState, Key1, Key2),
    case NGameState of
	nogame -> Pid1 ! {frame, quit},
		  Pid2 ! {frame, quit},
		  ok;
	_      -> Pid1 ! {frame, NGameState},
		  Pid2 ! {frame, flip_GameState(NGameState)},
		  timer:sleep(500),
		  game_loop(NGameState, Pid1, Pid2)
    end.


receive_inputs(Pid1, Pid2, none, none) ->
    receive
	{Pid1, Key1}  -> receive_inputs(Pid1, Pid2, Key1, none);
	{Pid2, left}  -> receive_inputs(Pid1, Pid2, none, right);
	{Pid2, right} -> receive_inputs(Pid1, Pid2, none, left)
    after
	1000 -> exit(timeout)
    end;
receive_inputs(_, Pid2, Key1, none) ->
    receive
	{Pid2, Key2} -> {Key1, Key2}
    after
	1000 -> exit(timeout)
    end;
receive_inputs(Pid1, _, none, Key2) ->
    receive
	{Pid1, Key1} -> {Key1, Key2}
    after
	1000 -> exit(timeout)
    end.


update_GameState({gamestate, Field_Size,
		 Ball,
		 {bot_player, P1_Pos},
		 {top_player, P2_Pos}}, Key1, Key2) ->
    NP1_Pos = update_player(Field_Size, P1_Pos, Key1),
    NP2_Pos = update_player(Field_Size, P2_Pos, Key2),
    NBall   = update_ball(Field_Size, Ball, NP1_Pos, NP2_Pos),
    case NBall of
	noball -> nogame;
	_      -> {gamestate, Field_Size, NBall,
		   {bot_player, NP1_Pos},
		   {top_player, NP2_Pos}}
    end.


update_player({_, _}, {Px, Py}, left) when Px > 3->
    {Px-1, Py};
update_player({Gx, _}, {Px, Py}, right) when Px < (Gx - 4)->
    {Px+1, Py};
update_player(_, Pos, _) ->
    Pos.

update_ball(_, {ball, {_, 0}, _}, _, _)->
    noball;
update_ball({_, Gy}, {ball, {_, Py}, _}, _, _) when Py == Gy-1 ->
    noball;
update_ball(Grid, {ball, {PBx, PBy}, DirBall},
	    {P1x, P1y}, _) when PBy == P1y-1 ->
    update_ball_aux(Grid, {ball, {PBx, PBy}, DirBall},
		    {P1x, P1y});
update_ball(Grid, {ball, {PBx, PBy}, DirBall},
	    _, {P2x, P2y}) when PBy == P2y+1 ->
    update_ball_aux(Grid, {ball, {PBx, PBy}, DirBall},
		    {P2x, P2y});
update_ball({Gx, _},
	    {ball, {PBx, PBy}, {DBx, DBy}},
	    _, _) when PBx == Gx-1 ->
    {ball, {PBx-DBx, PBy+DBy}, {-DBx, DBy}};
update_ball(_,
	    {ball, {0, PBy}, {DBx, DBy}},
	    _, _) ->
    {ball, {1 , PBy+DBy}, {-DBx, DBy}};
update_ball(_, {ball, {PBx, PBy}, {DBx, DBy}}, _, _) ->
    {ball, {PBx+DBx, PBy+DBy}, {DBx, DBy}}.
    

update_ball_aux(_, {ball, {PBx, PBy}, {DBx, DBy}},
		{PBx, _}) ->
    {ball, {PBx+DBx, PBy-DBy}, {DBx, -DBy}};
update_ball_aux(_, {ball, {PBx, PBy}, {DBx, DBy}},
		{Px, _}) when PBx+DBx >= Px-2, PBx < Px ->
    case PBx of
	0 -> {ball, {1, PBy-DBy}, {1, -DBy}};
	_ -> {ball, {PBx-1, PBy-DBy}, {-1, -DBy}}
    end;
update_ball_aux({Gx, _},
	    {ball, {PBx, PBy}, {DBx, DBy}},
	    {Px, _}) when PBx+DBx =< Px+2, PBx > Px ->
    Max_x = Gx-1,
    case PBx of
	Max_x -> {ball, {Max_x-1, PBy-DBy}, {-1, -DBy}};
	_     -> {ball, {PBx+1, PBy-DBy}, {1, -DBy}}
    end;
update_ball_aux({Gx, _},
		{ball, {PBx, PBy}, {DBx, DBy}},
		_) ->
    Max_x = Gx-1,
    case PBx of
	0     -> {ball, {1, PBy+DBy}, {-DBx, DBy}};
	Max_x -> {ball, {Max_x-1, PBy+DBy}, {-DBx, DBy}};
	_     -> {ball, {PBx+DBx, PBy+DBy}, {DBx, DBy}}
    end.


flip_GameState({gamestate, {Gx, Gy},
		{ball, {Px, Py}, {Dx, Dy}},
		{bot_player, {P1x, _}},
		{top_player, {P2x, _}}}) ->
    {gamestate, {Gx, Gy},
     {ball, {Gx-1-Px, Gy-1-Py}, {-Dx,-Dy}},
     {bot_player, {Gx-1-P1x, Gy-1}},
     {top_player, {Gx-1-P2x, 0}}}.


stop_server() ->
    serverpong ! stop.


 

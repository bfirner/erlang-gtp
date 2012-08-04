%
% Copyright (c) 2012 Bernhard Firner
% All rights reserved.
%
% This program is free software; you can redistribute it and/or
% modify it under the terms of the GNU General Public License
% as published by the Free Software Foundation; either version 2
% of the License, or (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
% or visit http://www.gnu.org/licenses/gpl-2.0.html
%
-module(gtp).
-vsn(0.1).
-compile(export_all).

%Go text protocol implementation for erlang
%Specifications for this protocol can be found at:
%http://www.lysator.liu.se/~gunnar/gtp/gtp2-spec-draft2/gtp2-spec.html

%A record for the current game state
-record(game_state,
  { black_prisoners = 0,
    white_prisoners = 0,
    moves = [],
    board = twod_array:new(19, empty),
    white_groups = [],
    black_groups = [],
    komi = 7.5,
    main_time = 0,
    byo_yomi_time = 0,
    byo_yomi_stones = 0,
    cur_by_time = 0,
    cur_by_stones = 0}).


%Set the game state to match the given record
set_game_state(State) ->
  put(black_prisoners, State#game_state.black_prisoners),
  put(white_prisoners, State#game_state.white_prisoners),
  put(moves, State#game_state.moves),
  put(board, State#game_state.board),
  put(white_groups, State#game_state.white_groups),
  put(black_groups, State#game_state.black_groups),
  put(komi, State#game_state.komi),
  put(main_time, State#game_state.main_time),
  put(byo_yomi_time, State#game_state.byo_yomi_time),
  put(byo_yomi_stones, State#game_state.byo_yomi_stones),
  put(cur_by_time, State#game_state.cur_by_time),
  put(cur_by_stones, State#game_state.cur_by_stones).

get_game_state() ->
  #game_state{
    black_prisoners = get(black_prisoners),
    white_prisoners = get(white_prisoners),
    moves = get(moves),
    board = get(board),
    white_groups = get(white_groups),
    black_groups = get(black_groups),
    komi = get(komi),
    main_time = get(main_time),
    byo_yomi_time = get(byo_yomi_time),
    byo_yomi_stones = get(byo_yomi_stones),
    cur_by_time = get(cur_by_time),
    cur_by_stones = get(cur_by_stones)}.

%To preprocess the string we should remove all occurences of control characters
%This includes newlines and such.
%Remove anything following a hash sign (#) as that is commented text
%Also convert all tabs to spaces and strip out excess whitespace
preprocess(Str) ->
  Not_comment = fun(C) -> (C /= $\n) and (C /= $#) end,
  Tab_to_space = fun(C, Rest) ->
      Rest ++ (if (C == $\t) -> " "; true -> [C] end) end,
  string:strip(lists:foldl(Tab_to_space, "", lists:takewhile(Not_comment, Str))).

%Convert a numeric coordinate (1, 2) to a string coordinate (A2)
coordinate_to_string(X, Y) ->
  Xval = 64 + if (X < 9) -> X; true -> X+1 end,
  binary_to_list(<<(Xval)>>) ++ integer_to_list(Y).

%Convert a string coordinate to a numeric coordinate tuple
list_to_coordinate([A | B]) ->
  Aval = string:to_upper(A) - if (A < "I") -> 64; true -> 65 end,
  {Aval, list_to_integer(B)}.

pairs_to_string(Pairs) ->
  lists:foldl(fun ({X, Y}, Acc) -> if
          (Acc /= "") -> Acc ++ ", " ++ coordinate_to_string(X, Y);
          true -> coordinate_to_string(X, Y) end end, "", Pairs).

%get_group_at({X, Y}) -> Stones
%Returns the group of stones at the given location, or an empty group of
%stones if there are no stones there.
get_group_at({X, Y}) ->
  White = lists:filter(fun (Group) -> stones:is_stone({X, Y}, Group) end, get(white_groups)),
  Black = lists:filter(fun (Group) -> stones:is_stone({X, Y}, Group) end, get(white_groups)),
  Wlen = length(White),
  Blen = length(Black),
  if (Wlen /= 0) ->
      hd(White);
    (Blen /= 0) ->
      hd(Black);
    true ->
      stones:new()
  end.

get_groups_at(Points) ->
  Anymatch = fun (Group) -> lists:any( fun (Stone) -> stones:is_stone(Stone, Group) end, Points) end,
  White = lists:filter(Anymatch, get(white_groups)),
  Black = lists:filter(Anymatch, get(black_groups)),
  Wlen = length(White),
  Blen = length(Black),
  if (Wlen /= 0) ->
      White;
    (Blen /= 0) ->
      Black;
    %Otherwise return an empty list
    true ->
      []
  end.

partition_groups_at(Points, Groupname) ->
  Anymatch = fun (Group) -> lists:any( fun (Stone) -> stones:is_stone(Stone, Group) end, Points) end,
  lists:partition(Anymatch, get(Groupname)).

%Return a list of adjacent spaces. Some may be off of the board
adjacent({X, Y}) ->
  [{X-1, Y}, {X+1, Y}, {X, Y-1}, {X, Y+1}].

distance({X1, Y1}, {X2, Y2}) ->
  abs(X1 - X2) + abs(Y1 - Y2).

%Check if a stone is adjacent to a group
is_adjacent({X, Y}, Group) ->
  stones:is_stone({X, Y+1}, Group) or
  stones:is_stone({X, Y-1}, Group) or
  stones:is_stone({X-1, Y}, Group) or
  stones:is_stone({X+1, Y}, Group).

%Get the stones where the two groups touch
get_adjacent(GroupA, GroupB) ->
  stones:fold_stones(fun (Stone, Acc) -> IsAdj = is_adjacent(Stone, GroupB), if (IsAdj) -> [Stone] ++ Acc;
          true -> Acc end end, [], GroupA).

%Convenience function to set a stone on the board
set_stone({X, Y}, Name) ->
  put(board, twod_array:set(X, Y, Name, get(board))).

%Convenience function to clear the board
clear_board() ->
  Size = twod_array:size(get(board)),
  put(board, twod_array:new(Size, empty)).

%Remove any kos marked on the board
remove_ko() ->
  Board = get(board),
  Boardsize = twod_array:size(Board),
  AllStones = [{XX, YY} || XX <- lists:seq(1, Boardsize), YY <- lists:seq(1, Boardsize)],
  lists:map(fun ({XX, YY}) -> IsKo = twod_array:get(XX, YY, Board) == ko,
        if (IsKo) -> put(board, twod_array:set(XX, YY, empty, Board)); true -> true end end, AllStones).

%Add a new stone to the board, combining it with neighbor groups
%and removing liberties from adjacent opponent groups
add_stone(Color, {X, Y}) ->
  %Remember which stones are allies and which are enemies
  AllyName = if (Color == black) -> black_groups; true -> white_groups end,
  EnemyName = if (Color == black) -> white_groups; true -> black_groups end,
  set_stone({X, Y}, Color),
  remove_ko(),
  EnemyColor = if (Color == black) -> white; true -> black end,
  Allystones = [{X2, Y2} || {X2, Y2} <- adjacent({X, Y}),
    twod_array:get(X2, Y2, get(board)) == Color],
  Enemystones = [{X2, Y2} || {X2, Y2} <- adjacent({X, Y}),
    twod_array:get(X2, Y2, get(board)) == EnemyColor],
  %io:format("Ally stones are:~n"),
  %lists:foreach(fun({X2, Y2}) -> io:format("~s~n", [coordinate_to_string(X2, Y2)]) end, Allystones),
  %io:format("Enemy stones are:~n"),
  %lists:foreach(fun({X2, Y2}) -> io:format("~s~n", [coordinate_to_string(X2, Y2)]) end, Enemystones),
  %Don't need to check for a ko here since we just cleared it if it existed
  Liberties = [{X2, Y2} || {X2, Y2} <- adjacent({X, Y}),
    (twod_array:get(X2, Y2, get(board)) == empty)],
  %Find groups at those locations. Remember all groups to modify the board state later.
  {CloseAllies, FarAllies} = partition_groups_at(Allystones, AllyName),
  {CloseEnemies, FarEnemies} = partition_groups_at(Enemystones, EnemyName),
  %io:format("CloseAllies are:~n"),
  %lists:foreach(fun(Ally) -> io:format("~s~n", [pairs_to_string(stones:stones_to_list(Ally))]) end, CloseAllies),
  %io:format("CloseEnemies are:~n"),
  %lists:foreach(fun(Enemy) -> io:format("~s~n", [pairs_to_string(stones:stones_to_list(Enemy))]) end, CloseEnemies),
  %Remove this stone from the liberty list of these ally and enemy groups
  Group_rm_liberty = fun (Group) -> stones:del_liberty({X, Y}, Group) end,
  Allies = lists:map(Group_rm_liberty, CloseAllies),
  Enemies = lists:map(Group_rm_liberty, CloseEnemies),
  %Combine this group with neighbor groups
  Stonegroup = stones:new([{X, Y}], Liberties),
  Newgroup = lists:foldl(fun (A, Acc) -> stones:join(A, Acc) end, Stonegroup, Allies),
  %io:format("New group is:~n"),
  %io:format("~s~n", [pairs_to_string(stones:stones_to_list(Newgroup))]),
  Newgrouplen = stones:size(Newgroup),
  Enlen = length(Enemies),
  %If the liberty list of an enemy group hits zero remove it and adjust liberties
  {Left, Killed} = lists:partition(fun (Group) -> 0 /= stones:liberties(Group) end, Enemies),
  %Add these groups back into the world state
  %Update ally groups
  put(AllyName, [Newgroup] ++ FarAllies),
  %If no groups were killed just add these groups back into the model.
  %If groups were killed then liberties of ally groups need to be adjusted
  Numkilled = length(Killed),
  if (0 == Numkilled) ->
      if (Enlen /= 0) ->
          put(EnemyName, Enemies ++ FarEnemies);
        true -> true
      end;
    %Enemy groups were killed -> update ally liberties and possibly mark ko
    true ->
      AllKilled = lists:foldl(fun (Stones, Acc) -> stones:join(Stones, Acc) end, stones:new(), Killed),
      NewEmpty = stones:stones_to_list(AllKilled),
      %Add these new empty spots to the liberty counts of adjacent groups
      %Find all spaces adjacent to the killed groups so that we can find the adjacent groups
      %The group made with the current move was added into the global state previously so
      %just use get to access all ally groups.
      Adjacents = stones:fold_stones(fun (El, Acc) -> Acc ++ adjacent(El) end, [], AllKilled),
      ChangeStones = [{X2, Y2} || {X2, Y2} <- Adjacents, twod_array:get(X2, Y2, get(board)) == Color],
      {Changed, Unchanged} = partition_groups_at(ChangeStones, AllyName),
      %Increase liberties whenever a killed stone is adjacent to a changed group
      IncLibs = fun (Stones) -> Adjs = get_adjacent(Stones, AllKilled),
          lists:foldl(fun (Stone, Prev) -> stones:add_liberty(Stone, Prev) end, Stones, Adjs) end,
      Updated = lists:map(IncLibs, Changed),
      %Set ally groups
      put(AllyName, Updated ++ Unchanged),
      %Update enemy groups - the groups that weren't killed and the far away groups
      put(EnemyName, Left ++ FarEnemies),

      %Now update the map
      %If one stone was removed mark its position with a ko if a recapture would repeat the board position
      %Otherwise mark the position as empty
      Stonesremoved = length(NewEmpty),
      if ((Newgrouplen == 1) and (Stonesremoved == 1)) ->
          set_stone(hd(NewEmpty), ko);
        true ->
          lists:map(fun (Coord) -> set_stone(Coord, empty) end, NewEmpty)
      end
  end.

do_command([]) ->
  false.
do_command([Cmd | Args], Num, Engine) ->
  Known = ["protocol_version", "name", "version", "known_command", "list_commands", "quit",
           "boardsize", "query_boardsize", "clear_board", "komi", "fixed_handicap",
           "place_free_handicap", "set_free_handicap",
           "play", "genmove", "reg_genmove", "undo",
           "loadsgf"],
  Matches = fun(X) -> X == hd(Args) end,
  io:format("Handling command ~s~n", [Cmd]),
  Succ = "=" ++ Num ++ " ",
  Fail = "?" ++ Num ++ " ",
  Numargs = length(Args),
  Response = case Cmd of
    %Administrative commands:
    "protocol_version" ->
      Succ ++ "2";
    "name" ->
      Succ ++ "Erlang GTP";
    "version" ->
      Succ ++ "0.1";
    "known_command" ->
      if (Numargs == 1) ->
          CmdKnown = lists:any(Matches, Known),
          if (CmdKnown) -> Succ ++ "true"; true -> Succ ++ "false" end;
        %Fail if there was no argument
        true -> Fail end;
    "list_commands" ->
      Succ ++ lists:foldl(fun(S, Accum) -> Accum ++ "\n" ++ S end, hd(Known), tl(Known));
    "quit" ->
      io:format("~s", [Succ ++ "\n\n"]),
      erlang:halt(0);

    %Setup commands
    "boardsize" ->
      if (Numargs == 1) ->
          %Set the boardsize in the process dictionary
          {Size, _} = string:to_integer(hd(Args)),
          put(board, twod_array:new(Size, empty)),
          Succ;
        %Fail if there was no argument
        true -> Fail end;
    "query_boardsize" ->
      Boardsize = twod_array:size(get(board)),
      Succ ++ integer_to_list(Boardsize);
    "clear_board" ->
      %Move history and captured stones are cleared
      %TODO Does this clear komi?
      CurState = get_game_state(),
      set_game_state(CurState#game_state{moves = [], black_prisoners = 0, white_prisoners = 0}),
      clear_board(),
      Succ;
    "komi" ->
      if (Numargs == 1) ->
          %Set komi in the process dictionary
          {Points, _} = string:to_integer(hd(Args)),
          put(komi, Points),
          Succ;
        %Fail if there was no argument
        true -> Fail end;
    "fixed_handicap" ->
      Moves = length(get(moves)),
      Bsize = twod_array:size(get(board)),
      Even = (Bsize rem 2) == 0,
      if (Moves /= 0) ->
          Fail ++ "board not empty";
        (Numargs == 1) ->
          %Place the given number of stones
          {Handi, _} = string:to_integer(hd(Args)),
          Center = (Bsize div 2) + 1,
          if ((Even and (Handi > 4)) or
              ((Bsize < 7) and (Handi > 0)) or
              (Handi > 9)) ->
              Fail ++ "invalid number of stones";
            %This is a valid handicap arrangement
            true ->
              MinSpace = if (Bsize > 12) -> 4; true -> 3 end,
              MaxSpace = if (Bsize > 12) -> Bsize - 3; true -> Bsize - 2 end,

              %Place komi stones as specified by section 4.1.1 of the GTP
              (Handi >= 1) andalso
                  put(board, twod_array:set(MinSpace, MinSpace, black, get(board))),
              (Handi >= 2) andalso
                  put(board, twod_array:set(MaxSpace, MaxSpace, black, get(board))),
              (Handi >= 3) andalso
                  put(board, twod_array:set(MinSpace, MaxSpace, black, get(board))),
              (Handi >= 4) andalso
                  put(board, twod_array:set(MaxSpace, MinSpace, black, get(board))),
              %Center stone occurs at 5, 7, and 9 handicaps
              ((Handi == 5) or (Handi == 7) or (Handi == 9)) andalso
                  put(board, twod_array:set(Center, Center, black, get(board))),
              (Handi >= 6) andalso
              begin put(board, twod_array:set(MinSpace, Center, black, get(board))),
                  put(board, twod_array:set(MaxSpace, Center, black, get(board)))
              end,
              (Handi >= 8) andalso
              begin put(board, twod_array:set(Center, MinSpace, black, get(board))),
                  put(board, twod_array:set(Center, MaxSpace, black, get(board)))
              end,
              Curboard = get(board),
              Blackcoords = [{X, Y} ||
                X <- lists:seq(1, Bsize), Y <- lists:seq(1, Bsize),
                twod_array:get(X, Y, Curboard) == black],
              PrintCoords = lists:map(fun({X, Y}) -> coordinate_to_string(X, Y) end, Blackcoords), 
              Succ ++ string:join(PrintCoords, ", ") end;
        %Fail if there was no argument
        true -> Fail end;
    "place_free_handicap" ->
      Fail;
    "set_free_handicap" ->
      Fail;
    "play" ->
      if (Numargs == 2) ->
          PlayColor = string:to_lower(hd(Args)),
          Color = if ("black" == PlayColor) -> black;
            "white" == PlayColor -> white;
            true -> unknown
          end,

          if (unknown == PlayColor) ->
              Fail ++ "Unrecognized color given";
            true ->
              IsPass = "PASS" == string:to_upper(hd(tl(Args))),
              if (IsPass) ->
                  remove_ko,
                  Succ;
                true ->
                  {X, Y} = list_to_coordinate(hd(tl(Args))),
                  io:format("Playing move ~s\n", [hd(tl(Args))]),
                  put(moves, get(moves) ++ [{X, Y, PlayColor}]),
                  %%Adding this stone in changes the board
                  %%but there may also be captures so the prisoner
                  %%counts must be modified.
                  %Check to see if this stone touches a group of
                  %the opposite color that now has zero liberties.
                  %Remove any dead stones of the apposing color.
                  %TODO Check for self capture.
                  add_stone(Color, {X, Y}),
                  Succ
              end
          end;
        true ->
          Fail
      end;
    "genmove" ->
      if (Numargs /= 1) ->
          Fail;
        true ->
          PlayColor = string:to_lower(hd(Args)),
          Color = if ("black" == PlayColor) -> black;
            "white" == PlayColor -> white;
            true -> unknown
          end,

          if (unknown == PlayColor) ->
              Fail ++ "Unrecognized color given";
            true ->
              ToPlay = Engine(get_game_state(), Color),
              IsPass = "PASS" == string:to_upper(ToPlay),
              IsResign = "resign" == string:to_upper(ToPlay),
              if (IsPass) ->
                  remove_ko,
                  Succ ++ ToPlay;
                (IsResign) ->
                  Succ ++ ToPlay;
                %Otherwise actually play the move
                true ->
                  {X, Y} = list_to_coordinate(ToPlay),
                  io:format("Playing move ~s\n", [ToPlay]),
                  put(moves, get(moves) ++ [{X, Y, PlayColor}]),
                  %TODO Check for self capture.
                  add_stone(Color, {X, Y}),
                  Succ ++ ToPlay
              end
          end
      end;
    "reg_genmove" ->
      if (Numargs /= 1) ->
          Fail;
        true ->
          PlayColor = string:to_lower(hd(Args)),
          Color = if ("black" == PlayColor) -> black;
            "white" == PlayColor -> white;
            true -> unknown
          end,

          if (unknown == PlayColor) ->
              Fail ++ "Unrecognized color given";
            true ->
              ToPlay = Engine(get_game_state(), Color),
              %Don't need to play the move, just need to generate it
              Succ ++ ToPlay
          end
      end;
    "undo" ->
      Fail;
    "print_board" ->
      %Print out the board using B and W for black and white pieces, + for empty and K for ko.
      CurBoard = get(board),
      CurBoardSize = twod_array:size(CurBoard),
      Rows = lists:seq(CurBoardSize, 1, -1),
      Columns = lists:seq(1, CurBoardSize),
      PrintCoord = fun (Val) -> PString = case Val of white -> "W";
            black -> "B";
            ko -> "K";
            empty -> "+";
            _ -> "?" end,
          io:format("~s", [PString]) end,
      %Print out a row of the board
      PrintRow = fun (Row) -> lists:map( fun (Col) -> PrintCoord(twod_array:get(Col, Row, CurBoard)) end, Columns), io:format("~n") end,
      lists:map(PrintRow, Rows),
      Succ;

    %Match unknown commands to an unknown command message.
    _ -> Fail ++ "unknown command"
  end,
  io:format("~s", [Response ++ "\n\n"]).

control_loop(Engine) ->
  %Get the next command from standard input
  Cmd = preprocess(io:get_line("")),
  io:format("Got command: ~s~n", [Cmd]),
  Stlen = string:len(Cmd),
  io:format("Stlen is ~B~n", [Stlen]),
  if (Stlen > 0) ->
      %Tokenize the command
      Tokens = string:tokens(Cmd, " "),
      %Check if this command has a number
      Hasnum = error /= element(1, string:to_integer(hd(Tokens))),
      if (Hasnum) -> io:format("Got number ~s~n", [hd(Tokens)]), do_command(tl(Tokens), hd(Tokens), Engine);
        true -> do_command(Tokens, "", Engine)
      end;
    true -> true
  end,
  %io:format("~s", [Response]),
  control_loop(Engine).

%Default engine always passes
%Engines are written like this: engine(GameState, PlayColor)
empty_engine(_, _) ->
  "PASS".

main() ->
  %Make a default gamestate for the game and store it in the process dictionary
  set_game_state(#game_state{}),
  %Use the default engine if a move is asked for
  io:format("default engine returns ~s~n", [empty_engine(empty, empty)]),
  control_loop(fun empty_engine/2).


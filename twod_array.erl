-module(twod_array).
-compile(export_all).

%This module exports code for square two dimensional arrays.
%These indices of these arrays start at 1.
%This module does not restrict the name of items placed on the board,
%but a standard set for a game of go/baduk/weiqi would be:
%black, white, empty, and ko
%If the status of a position off of the board is requested then
%off_board is returned.

-record(twod, {size, arr}).

%2D arrays are stored as flat arrays of fixed size
new(Size) ->
  #twod{size = Size,
    arr = array:new([{size, Size * Size}, {fixed, true}])}.
new(Size, Default) ->
  #twod{size = Size,
    arr = array:new([{size, Size * Size}, {fixed, true}, {default, Default}])}.

%Return off_board if this is off of the board, and the status
%of the given location otherwise
get(X, Y, Array) ->
  if ((X =< 0) or (Y =< 0) or (X > Array#twod.size) or (Y > Array#twod.size)) ->
      off_board;
    true ->
      array:get((X-1) + (Y-1) * Array#twod.size, Array#twod.arr)
  end.

set(X, Y, Value, Array) ->
  New_array = array:set((X-1) + (Y-1) * Array#twod.size, Value, Array#twod.arr),
  Array#twod{arr = New_array}.

reset(X, Y, Array) ->
  New_array = array:reset((X-1) + (Y-1) * Array#twod.size, Array#twod.arr),
  Array#twod{arr = New_array}.

%Return the one-dimensional size of the array
size(Array) ->
  Array#twod.size.


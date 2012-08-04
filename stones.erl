-module(stones).
-compile(export_all).

%This module has code to support handling groups of stones.
%This includes counting liberties and combining groups.
%Stones and liberties are stored as ordsets so users can
%quickly check if a group contains a stone or liberty.

-record(group, {stones, liberties}).

%Take a list of stones and liberties and turn them into a stones record
%The stones and liberties should be coordinate pairs
new(Stones, Liberties) ->
  #group{stones = ordsets:from_list(Stones),
    liberties = ordsets:from_list(Liberties)}.
new() ->
  #group{stones = ordsets:new(),
    liberties = ordsets:new()}.

%Join a single stone with an existing group.
%join(Group, Group) -> Group
join(A, B) ->
  #group{stones = ordsets:union(A#group.stones, B#group.stones),
    liberties = ordsets:union(A#group.liberties, B#group.liberties)}.

size(Group) ->
  ordsets:size(Group#group.stones).

liberties(Group) ->
  ordsets:size(Group#group.liberties).

is_stone(Stone, Group) ->
  ordsets:is_element(Stone, Group#group.stones).

is_liberty(Liberty, Group) ->
  ordsets:is_element(Liberty, Group#group.liberties).

del_liberty(Liberty, Group) ->
  Newlibs = ordsets:del_element(Liberty, Group#group.liberties),
  Group#group{liberties = Newlibs}.

add_liberty(Liberty, Group) ->
  Newlibs = ordsets:add_element(Liberty, Group#group.liberties),
  Group#group{liberties = Newlibs}.

fold_stones(Fun, Acc0, Group) ->
  ordsets:fold(Fun, Acc0, Group#group.stones).

stones_to_list(Group) ->
  ordsets:to_list(Group#group.stones).

liberties_to_list(Group) ->
  ordsets:to_list(Group#group.liberties).


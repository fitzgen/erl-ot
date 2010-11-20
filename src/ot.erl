-module(ot).
-export([xform/2, apply_op/2]).


%% ## `xform`
%%
%% The following description of the `xform` function is taken from
%% [High-Latency, Low-Bandwidth Windowing in the Jupiter Collaboration
%% System][jupiterwin].
%%
%% > The general tool for handling conflicting messages is a function, `xform`,
%% > that maps a pair of messages to the fixed up versions. We write
%% >
%% >     xform(c, s) = {c’, s’}
%% >
%% > where c and s are the original client and server messages. The messages c’
%% > and s’ must have the property that if the client applies c followed by s’,
%% > and the server applies s followed by c’, then the client and server will
%% > wind up in the same final state.
%%
%% [jupiterwin]: ftp://ftp.lambda.moo.mud.org/pub/MOO/papers/JupiterWin.ps

%% ### Delete/Delete

xform({del, X}, {del, X}) ->
    {ok, {nop, nop}};
xform({del, X}, {del, Y}) when X > Y ->
    {ok, {{del, X-1}, {del, Y}}};
xform({del, X}, {del, Y}) when Y > X ->
    {ok, {{del, X}, {del, Y-1}}};

%% ### Insert/Insert
%%
%% Each insert operation has the form `{ins, Index, Character}`. We break ties
%% based on order of the characters, and always keep user data by shifting
%% insertion indices rather than creating deletion operations.
%%
%% TODO: What if we wanted to make insertion operations have the structure
%% `{ins, Index, String}`? Then we will need to take string length in to account
%% and try and merge the first parts of strings if they are identical for the
%% first N characters.

xform({ins, X, A}, {ins, X, A}) ->
    {ok, {nop, nop}};
xform({ins, X, A}, {ins, X, B}) when A < B->
    {ok, {{ins, X, A}, {ins, X+1, B}}};
xform({ins, X, A}, {ins, X, B}) when A > B->
    {ok, {{ins, X+1, A}, {ins, X, B}}};
xform({ins, X}, {ins, Y}) when X > Y ->
    {ok, {{ins, X+1}, {ins, Y}}};
xform({ins, X}, {ins, Y}) when Y > X ->
    {ok, {{ins, X}, {ins, Y+1}}};

%% ### Insert/Delete
%%
%% What do we do when one operation is an insert and the other is a delete? We
%% always attempt to keep the insert so that we do not lose user data. It is
%% always better to force users to go back and delete data to clean a document
%% up than it is to lose data.

xform({ins, X, A}, {del, Y}) when X =< Y ->
    {ok, {{ins, X, A}, {del, Y+1}}};
xform({ins, X, A}, {del, Y}) when X > Y->
    {ok, {{ins, X-1, A}, {del, Y}}};
xform(P, Q) ->
    xform(Q, P).


%% ## `apply_op`
%%
%% `apply_op` takes two arguments: a document (which is currently just a string),
%% and an operation. It returns a new document which is identical to the first,
%% accept with the modifications described in the operation applied to it.

apply_op(Doc, {del, X}) ->
    Doc_Length = length(Doc),
    lists:sublist(Doc, X-1) ++ lists:sublist(Doc, X+1, Doc_Length);
apply_op(Doc, {ins, 0, Char}) ->
    [Char] ++ Doc;
apply_op([H|T], {ins, X, Char}) ->
    [H | apply_op(T, {ins, X-1, Char})].



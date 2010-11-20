-module(ot).
-include_lib("eunit/include/eunit.hrl").
-export([xform/2, apply_op/2]).


%% This is a simple example written in Erlang of the real-time
%% group-collaboration algorithm [Operational Transformation][ot]. OT is an
%% optimistic algorithm that lets clients apply operations to a document
%% immediately as they are created, and only synchronizes the changes with the
%% server after they have been made. If another client has altered the document
%% while the first client was making the first operation, we can transform each
%% operation so that the server and every client will be brought back to the
%% same document state.
%%
%% Each operation is either an insert or a delete of a single character at an
%% index in a document. A document is simply a string. This is hardly fit for
%% real-world use: most documents are more complicated than plain text, and
%% working one character at a time will create an unneccessary amount of
%% operations which require transformations. The large amount of operations
%% taxes bandwitdh more than you would want in a real world application, as
%% well. I am using single character operations for simplicity.
%%
%% [ot]: http://en.wikipedia.org/wiki/Operational_transformation


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
%% In our simple example, there are only 3 possible pairs of operation types
%% which must be unified with eachother: delete/delete, insert/insert, and
%% insert/delete.
%%
%% [jupiterwin]: ftp://ftp.lambda.moo.mud.org/pub/MOO/papers/JupiterWin.ps

%% ### Delete/Delete
%%
%% The Delete/Delete pair is the easiest case to transform. If both operations
%% deleted the same character, then emit the `nop` (no operation) operation
%% because they are already in the same state. Otherwise, just shift the index
%% of the delete with the larger index down by one.

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

xform({ins, X, A}, {ins, X, A}) ->
    {ok, {nop, nop}};
xform({ins, X, A}, {ins, X, B}) when A < B->
    {ok, {{ins, X, A}, {ins, X+1, B}}};
xform({ins, X, A}, {ins, X, B}) when A > B->
    {ok, {{ins, X+1, A}, {ins, X, B}}};
xform({ins, X, A}, {ins, Y, B}) when X > Y ->
    {ok, {{ins, X+1, A}, {ins, Y, B}}};
xform({ins, X, A}, {ins, Y, B}) when Y > X ->
    {ok, {{ins, X, A}, {ins, Y+1, B}}};

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
xform({del, D}, {ins, X, C}) ->
    xform({ins, X, C}, {del, D});


xform(A, B) ->
    {error, {"Do not know how to transform:", A, B}}.


%% ## `apply_op`
%%
%% `apply_op` takes two arguments: a document (which is currently just a string),
%% and an operation. It returns a new document which is identical to the first,
%% accept with the modifications described in the operation applied to it.

apply_op(Doc, nop) ->
    Doc;
apply_op(Doc, {del, X}) ->
    Doc_Length = length(Doc),
    lists:sublist(Doc, X) ++ lists:sublist(Doc, X+2, Doc_Length);
apply_op(Doc, {ins, 0, Char}) ->
    [Char] ++ Doc;
apply_op([H|T], {ins, X, Char}) ->
    [H | apply_op(T, {ins, X-1, Char})].


%% ## Application
%%
%% TODO: http://www.erlang.org/doc/design_principles/applications.html


%% ## Tests

identical_delete_test() ->
    A = B = {del, 3},
    {ok, {Ap, Bp}} = xform(A, B),
    "foo" = apply_op(apply_op("food", A), Bp),
    "foo" = apply_op(apply_op("food", B), Ap).

identical_insert_test() ->
    A = B = {ins, 3, $d},
    {ok, {Ap, Bp}} = xform(A, B),
    "food" = apply_op(apply_op("foo", A), Bp),
    "food" = apply_op(apply_op("foo", B), Ap).

two_deletes_a_test() ->
    A = {del, 4},
    B = {del, 2},
    {ok, {Ap, Bp}} = xform(A, B),
    "erag" = apply_op(apply_op("erlang", A), Bp),
    "erag" = apply_op(apply_op("erlang", B), Ap).

two_deletes_b_test() ->
    A = {del, 2},
    B = {del, 4},
    {ok, {Ap, Bp}} = xform(A, B),
    "erag" = apply_op(apply_op("erlang", A), Bp),
    "erag" = apply_op(apply_op("erlang", B), Ap).

two_inserts_a_test() ->
    A = {ins, 1, $c},
    B = {ins, 3, $t},
    {ok, {Ap, Bp}} = xform(A, B),
    "scooter" = apply_op(apply_op("sooer", A), Bp),
    "scooter" = apply_op(apply_op("sooer", B), Ap).

two_inserts_b_test() ->
    A = {ins, 3, $t},
    B = {ins, 1, $c},
    {ok, {Ap, Bp}} = xform(A, B),
    "scooter" = apply_op(apply_op("sooer", A), Bp),
    "scooter" = apply_op(apply_op("sooer", B), Ap).

insert_delete_a_test() ->
    A = {ins, 2, $t},
    B = {del, 2},
    {ok, {Ap, Bp}} = xform(A, B),
    "cat" = apply_op(apply_op("cab", A), Bp),
    "cat" = apply_op(apply_op("cab", B), Ap).

insert_delete_b_test() ->
    A = {ins, 2, $t},
    B = {del, 0},
    {ok, {Ap, Bp}} = xform(A, B),
    "atb" = apply_op(apply_op("cab", A), Bp),
    "atb" = apply_op(apply_op("cab", B), Ap).

insert_delete_c_test() ->
    A = {ins, 0, $t},
    B = {del, 2},
    {ok, {Ap, Bp}} = xform(A, B),
    "tca" = apply_op(apply_op("cab", A), Bp),
    "tca" = apply_op(apply_op("cab", B), Ap).

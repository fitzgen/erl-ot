%% This is a simple example written in Erlang of the real-time
%% group-collaboration algorithm [Operational Transformation][ot]. OT is an
%% optimistic concurrency algorithm that lets clients apply operations to a
%% document immediately as they are created, and only synchronizes the changes
%% with the server after they have been made. If another client has altered the
%% document while the first client was making the first operation, we can
%% transform each operation so that the server and every client will be brought
%% back to the same document state.
%%
%% [ot]: http://en.wikipedia.org/wiki/Operational_transformation


-module(ot).
-include_lib("eunit/include/eunit.hrl").
-export([start/0, stop/0, server/1, client/1, get_doc/0]).


%% ## Operations
%%
%% Each operation is either an insert or a delete of a single character at an
%% index in a document. A document is simply a string. This is hardly fit for
%% real-world use: most documents are more complicated than plain text, and
%% working one character at a time will create an unneccessary amount of
%% operations which require transformations. The large amount of operations
%% taxes bandwitdh more than you would want in a real world application, as
%% well. I am using single character operations for simplicity.
%%
%% ### `xform`
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
%% > where `c` and `s` are the original client and server messages. The messages `c’`
%% > and `s’` must have the property that if the client applies `c` followed by `s’`,
%% > and the server applies `s` followed by `c’`, then the client and server will
%% > wind up in the same final state.
%%
%% In our simple example, there are only 6 possible pairs of operation types
%% which must be unified with eachother: nop/nop, delete/nop, insert/nop,
%% delete/delete, insert/insert, and insert/delete.
%%
%% [jupiterwin]: ftp://ftp.lambda.moo.mud.org/pub/MOO/papers/JupiterWin.ps

%% #### */`nop`
%%
%% Transforming a pair of operations when one is `nop` (no operation) is simple:
%% we don't really need to actually calculate the transformation anything.

xform(nop, nop) ->
    {ok, {nop, nop}};
xform(nop, {ins, X, A}) ->
    {ok, {{ins, X, A}, nop}};
xform({ins, X, A}, nop) ->
    {ok, {nop, {ins, X, A}}};
xform(nop, {del, X}) ->
    {ok, {{del, X}, nop}};
xform({del, X}, nop) ->
    {ok, {nop, {del, X}}};

%% #### Delete/Delete
%%
%% The Delete/Delete pair is the easiest case to transform, besides `nop`. If
%% both operations deleted the same character, then emit the `nop` operation
%% because they are already in the same state. Otherwise, just shift the index
%% of the delete with the larger index down by one.

xform({del, X}, {del, X}) ->
    {ok, {nop, nop}};
xform({del, X}, {del, Y}) when X > Y ->
    {ok, {{del, X-1}, {del, Y}}};
xform({del, X}, {del, Y}) when Y > X ->
    {ok, {{del, X}, {del, Y-1}}};

%% #### Insert/Insert
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

%% #### Insert/Delete
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
    {ok, {Bprime, Aprime}} = xform({ins, X, C}, {del, D}),
    {ok, {Aprime, Bprime}};


xform(A, B) ->
    {error, {"Do not know how to transform:", A, B}}.


%% ### `apply_op`
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

%% ## Messages and Client/Server Communication
%%
%% When passing our operations between client and server, we need to store some
%% metadata about the operation with it. We will call the combination of an
%% operation and its metadata a message.
%%
%% What kind of metadata do we need to store? Well we need to know an
%% operation's parent document state. It only makes sense to apply an operation
%% to a document which is in the same state as the document which generated the
%% operation was in.
%%
%% The easiest way to represent a document's state is a hash of its
%% contents. Daniel Spiewak sings the praises of using a hash to represent a
%% document's state (although, for compatibility with Google Wave, he is forced
%% into using a diferent technique):
%%
%% > This scheme has some very nice advantages. Given an operation (and its
%% associated parent hash), we can determine instantly whether or not we have
%% the appropriate document state to apply said operation. Hashes also have the
%% very convenient property of converging exactly when the document states
%% converge.
%%
%% [Daniel's article on understanding Operational
%% Transformation][understanding] is a must read for anyone interested in the
%% topic.
%%
%% We will represent messages with the structure `{msg, Parent, Operation}`.
%%
%% [understanding]: http://www.codecommit.com/blog/java/understanding-and-applying-operational-transformation

hash(Doc) ->
    binary_to_list(erlang:md5(Doc)).

apply_message(Doc, {msg, Parent, Op}) ->
    case hash(Doc) == Parent of
        true ->
            {ok, apply_op(Doc, Op)};
        false ->
            {error, "Cannot apply this message to the document because the
                     operation was generated from a different document state."}
    end.

server(Doc) ->
    server(Doc, [], []).

server(Doc, Clients, History) ->
    receive
        stop ->
            ?debugMsg("Server stopping~n"),
            lists:foreach(fun (C) -> C ! stop end, Clients),
            ok;
        {doc, Pid} ->
            Pid ! Doc,
            server(Doc, Clients, History);
        {newclient, Pid} ->
            ?debugFmt("Server got new client ~p~n", [Pid]),
            Pid ! {doc, Doc},
            server(Doc, [Pid | Clients], History);
        {msg, Parent, Op} ->
            % TODO: look in history
            ?debugFmt("Server got new message with op ~p~n", [Op]),
            case apply_message(Doc, {msg, Parent, Op}) of
                {ok, New_Doc} ->
                    ?debugMsg("    message applied successfully~n"),
                    broadcast(Clients, {msg, Parent, Op}),
                    server(New_Doc,
                           Clients,
                           [{hash(New_Doc), New_Doc} | History]);
                _ ->
                    ?debugMsg("    message did not apply, ignoring it...~n"),
                    server(Doc, Clients, History)
            end
    end.

broadcast([], _) ->
    ok;
broadcast([C|Clients], Msg) ->
    ?debugFmt("broadcasting ~p to ~p~n", [Msg, C]),
    C ! Msg,
    broadcast(Clients, Msg).

client(Server) ->
    Server ! {newclient, self()},
    receive
        {doc, Doc} ->
            Server ! {msg, hash(Doc), nop},
            client(Server, Doc, [{nop, Doc}])
    end.

% TODO: outgoing should be pairs of {Op, Doc}...
client(Server, Doc, Outgoing) ->
    receive
        {user_op, Op} ->
            ?debugFmt("Client ~p received user op ~p~n", [self(), Op]),
            case Outgoing of
                [] ->
                    ?debugMsg("    sending the new operation~n"),
                    Server ! {msg, hash(Doc), Op},
                    client(Server, Doc, [{Op, apply_op(Doc, Op)}]);
                _ ->
                    ?debugMsg("    queueing the new operation~n"),
                    client(Server, Doc, Outgoing ++ [{Op, apply_op(Doc, Op)}])
            end;
        {msg, Hash, Op} ->
            ?debugFmt("Client ~p got message from server: ~p~n", [self(), {msg, Hash, Op}]),
            Hash = hash(Doc),
            New_Doc = apply_op(Doc, Op),
            case Outgoing of
                [{Op, New_Doc} | Rest] ->
                    ?debugMsg("    it was one that I sent~n"),
                    case Rest of
                        [{Next_Op, _} | _] ->
                            ?debugFmt("Client ~p sending buffered operation ~p to server~n", [self(), Next_Op]),
                            Server ! {msg, hash(New_Doc), Next_Op},
                            client(Server, New_Doc, Rest);
                        [] ->
                            client(Server, New_Doc, [])
                    end;
                _ ->
                    ?debugMsg("    it was one someone else sent~n"),
                    Outgoing1 = transform_each(Outgoing, Op),
                    client(Server, New_Doc, Outgoing1)
            end
    end.

transform_each([], _) ->
    [];
transform_each([{A, Doc} | Rest], B) ->
    {ok, {Aprime, Bprime}} = xform(A, B),
    [{Aprime, Doc} | transform_each(Rest, Bprime)].


%% ## Application
%%
%% OTP Application interface to play nice with the Erlang ecosystem.
%%
%% TODO: http://www.erlang.org/doc/design_principles/applications.html

start() ->
    register(ot_server, spawn_link(ot, server, [""])).

stop() ->
    ot_server ! stop.

get_doc() ->
    get_doc(ot_server).

get_doc(Server) ->
    Server ! {doc, self()},
    receive
        Doc ->
            Doc
    end.


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

client_server_communication_test() ->
    Server = spawn_link(ot, server, [" is cool"]),
    Client = spawn_link(ot, client, [Server]),
    Client ! {user_op, {ins, 0, $n}},
    Client ! {user_op, {ins, 1, $i}},
    Client ! {user_op, {ins, 2, $c}},
    Client ! {user_op, {ins, 3, $k}},
    "nick" = get_doc(Server),
    Server ! stop.

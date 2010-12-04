%% This is a simple example of the real-time group-collaboration algorithm
%% [Operational Transformation][ot] (or OT) written in Erlang. OT is an
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
-export([start/0, stop/0, server/1, client/1, get_doc/0, get_doc/1]).


%% ## Operations: Transforming and Applying
%%
%% Each operation is either an insert or a delete of a single character at an
%% index in a document. A document is simply a string. This is hardly fit for
%% real-world use: most documents are more complicated than plain text, and
%% working one character at a time will create an unneccessary amount of
%% operations which require transformations. The large amount of operations
%% taxes bandwidth more than you would want in a real world application, as
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

%% ## Messages and Client <-> Server Communication
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


%% ### `server`
%%
%% The server simply keeps a master copy of the document, a list of clients
%% which are connected, and a history of changes. The history is not currently
%% used in this implementation; more about that later.
%%
%% The clients are constantly sending messages to the server, but the server can
%% only accept one at a time. It attempts to take them on a first come, first
%% serve basis, however it must reject many messages. To greatly simplify the
%% implementation, **the server will only accept messages whose parent hash is
%% equal to the hash of the current master document**. It ignores every other
%% message.
%%
%% Once it accepts a message which is based off of the current master document,
%% it sends broadcasts that document back to every client, including the one
%% which generated it. The reason it sends the message back to the client which
%% created it is so that the client gets confirmation that the operation was
%% applied and can send its next operation.
%%
%% We could make the OT process more efficient by accepting any message which
%% inherits from any part of the history, and apply `xform` as needed until we
%% get the document in a consistent state, but this would muddy the currently
%% clear logic of the server. Further, in Real World use cases, you want to keep
%% the work the server performs to a minimum. In such Real World use cases, the
%% client would probably be a browser running JavaScript, and efficiency will
%% matter less because it is only the server which is under load, and not the
%% client.

server(Doc) ->
    server(Doc, [], []).

server(Doc, Clients, History) ->
    receive
        stop ->
            lists:foreach(fun (C) -> C ! stop end, Clients),
            ok;
        {doc, Pid} ->
            Pid ! Doc,
            server(Doc, Clients, History);
        {newclient, Pid} ->
            Pid ! {doc, Doc},
            server(Doc, [Pid | Clients], History);
        {msg, Parent, Op} ->
            case apply_message(Doc, {msg, Parent, Op}) of
                {ok, New_Doc} ->
                    broadcast(Clients, {msg, Parent, Op}),
                    server(New_Doc,
                           Clients,
                           [{hash(New_Doc), New_Doc} | History]);
                _ ->
                    server(Doc, Clients, History)
            end
    end.

apply_message(Doc, {msg, Parent, Op}) ->
    case hash(Doc) == Parent of
        true ->
            {ok, apply_op(Doc, Op)};
        false ->
            {error, "Cannot apply this message to the document because the
                     operation was generated from a different document state."}
    end.

broadcast([], _) ->
    ok;
broadcast([C|Clients], Msg) ->
    C ! Msg,
    broadcast(Clients, Msg).

%% ### `client`

client(Server) ->
    Server ! {newclient, self()},
    receive
        {doc, Doc} ->
            client(Server, Doc, [])
    end.

client(Server, Doc, Outgoing) ->
    receive
        {user_op, Op} ->
            case Outgoing of
                [] ->
                    Server ! {msg, hash(Doc), Op},
                    client(Server, Doc, [{Op, apply_op(Doc, Op)}]);
                _ ->
                    {_, Last_Doc} = lists:last(Outgoing),
                    client(Server, Doc, Outgoing ++ [{Op, apply_op(Last_Doc, Op)}])
            end;
        {doc, Pid} ->
            case Outgoing of
                [] ->
                    Pid ! Doc,
                    client(Server, Doc, Outgoing);
                [_|_] ->
                    {_, Client_Doc} = lists:last(Outgoing),
                    Pid ! Client_Doc,
                    client(Server, Doc, Outgoing)
                end;
        {msg, Hash, Op} ->
            Hash = hash(Doc),
            New_Doc = apply_op(Doc, Op),
            case Outgoing of
                [{Op, New_Doc} | Rest] ->
                    case Rest of
                        [] ->
                            client(Server, New_Doc, []);
                        [{Next_Op, _} | _] ->
                            Server ! {msg, hash(New_Doc), Next_Op},
                            client(Server, New_Doc, Rest)
                    end;
                _ ->
                    Outgoing1 = transform_each(Outgoing, Op, New_Doc),
                    case Outgoing1 of
                        [] ->
                            client(Server, New_Doc, []);
                        [{Next_Op, _} | _] ->
                            Server ! {msg, hash(New_Doc), Next_Op},
                            client(Server, New_Doc, Outgoing1)
                    end
            end
    end.

transform_each([], _, _) ->
    [];
transform_each([{A, _} | Rest], B, New_Doc) ->
    {ok, {Aprime, Bprime}} = xform(A, B),
    Next_New_Doc = apply_op(New_Doc, Aprime),
    [{Aprime, Next_New_Doc} | transform_each(Rest, Bprime, Next_New_Doc)].


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
            {ok, Doc}
    after 5000 ->
            {error, "No response within 5 seconds."}
    end.


%% ## Tests

%% ### Applying and Transforming Operations

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

%% ### Client <-> Server Communication

simple_client_server_communication_test() ->
    Server = spawn_link(ot, server, [" is cool"]),
    Client = spawn_link(ot, client, [Server]),
    Client ! {user_op, {ins, 0, $n}},

    receive
    after 100 ->
            {ok, "n is cool"} = get_doc(Server)
    end,
    Client ! {user_op, {ins, 1, $i}},

    receive
    after 100 ->
            {ok, "ni is cool"} = get_doc(Server)
    end,

    Client ! {user_op, {ins, 2, $c}},
    receive
    after 100 ->
            {ok, "nic is cool"} = get_doc(Server)
    end,

    Client ! {user_op, {ins, 3, $k}},
    receive
    after 100 ->
            {ok, "nick is cool"} = get_doc(Server)
    end,

    Server ! stop.

concurrent_deletes_test() ->
    Server = spawn_link(ot, server, ["This is the document."]),
    A = spawn_link(ot, client, [Server]),
    B = spawn_link(ot, client, [Server]),

    % A deletes "is".
    A ! {user_op, {del, 6}},
    A ! {user_op, {del, 5}},

    % B deletes "the".
    B ! {user_op, {del, 10}},
    B ! {user_op, {del, 9}},
    B ! {user_op, {del, 8}},

    receive
    after 1000 ->
            {ok, "This   document."} = get_doc(A),
            {ok, "This   document."} = get_doc(B),
            {ok, "This   document."} = get_doc(Server)
    end,

    Server ! stop.

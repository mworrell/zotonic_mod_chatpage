-module(m_chatpage).

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    is_visible/2,

    get/2,
    recent/3,
    list/5,
    insert/5,
    insert/6,
    delete/3,
    delete_all/2,
    delete_all/3,
    install/1
]).

-include("zotonic.hrl").

-define(PAGELEN, 50).

m_find_value(list, #m{value=undefined} = M, _Context) ->
    M#m{value=list};
m_find_value(Sub, #m{value=list} = M, _Context) ->
    M#m{value={list,Sub}};
m_find_value(RscId, #m{value={list, Sub}}, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined -> undefined;
        Id ->
            case is_visible(Id, Context) of
                true -> list(Id, undefined, z_convert:to_binary(Sub), ?PAGELEN, Context);
                false -> undefined
            end
    end;
m_find_value(recent, #m{value=undefined} = M, _Context) ->
    M#m{value=recent};
m_find_value(Sub, #m{value=recent} = M, _Context) ->
    M#m{value={recent,Sub}};
m_find_value(RscId, #m{value={recent,Sub}}, Context) ->
    case is_visible(RscId, Context) of
        true -> recent(RscId, z_convert:to_binary(Sub), Context);
        false -> undefined
    end;
m_find_value(_, #m{}, _Context) ->
    undefined.

m_to_list(#m{}, _Context) ->
    [].

m_value(#m{}, _Context) ->
    undefined.


-spec is_visible(integer(), #context{}) -> boolean().
is_visible(RscId, Context) ->
    z_acl:rsc_visible(RscId, Context)
    andalso z_acl:is_allowed(use, mod_chatpage, Context).

-spec get(integer(), #context{}) -> list() | undefined.
get(MsgId, Context) ->
    z_db:assoc_row("
            select *
            from chatpage_message
            where id = $1
            ",
            [MsgId],
            Context).

-spec recent(integer(), binary(), #context{}) -> list() | undefined.
recent(RscId, Sub, Context) ->
    z_db:assoc_row("
            select *
            from chatpage_message
            where rsc_id = $1
              and sub = $2
            order by id desc
            limit 1
            ",
            [RscId, Sub],
            Context).


-spec list(integer(), binary(), integer()|undefined, integer(), #context{}) -> list().
list(RscId, Sub, undefined, PageLen, Context) ->
    z_db:assoc("
            select *
            from chatpage_message
            where rsc_id = $1
              and sub = $2
            order by id desc
            limit $3
            ",
            [RscId, Sub, PageLen],
            Context);
list(RscId, Sub, FromId, PageLen, Context) ->
    z_db:assoc("
            select *
            from chatpage_message
            where rsc_id = $1
              and sub = $2
              and id < $3
            order by id desc
            limit $4
            ",
            [RscId, Sub, FromId, PageLen],
            Context).
    

-spec insert(integer(), binary(), integer()|undefined, binary(), #context{}) ->
        {ok, integer()} | {error, term()}.
insert(RscId, Sub, UserId, Message, Context) ->
    insert(RscId, Sub, UserId, Message, undefined, Context).

-spec insert(integer(), binary(), integer()|undefined, binary()|undefined, binary(), #context{}) ->
        {ok, integer()} | {error, term()}.
insert(RscId, Sub, UserId, Message, Unique, Context) ->
    Props = [
        {rsc_id, RscId},
        {sub, Sub},
        {message, Message},
        {uniqueid, Unique}
        | case UserId of
            undefined -> [];
            _ -> [ { user_id, UserId } ]
          end
    ],
    z_db:insert(chatpage_message, Props, Context).

%% @doc Delete a specific message in a chat
-spec delete(integer(), integer(), #context{}) -> ok.
delete(RscId, MsgId, Context) ->
    z_db:q("delete from chatpage_message
            where rsc_id = $1
              and id = $2",
           [RscId, MsgId], Context),
    ok.

%% @doc Delete all messages in a chat
-spec delete_all(integer(), #context{}) -> ok.
delete_all(RscId, Context) ->
    z_db:q("delete from chatpage_message where rsc_id = $1", [RscId], Context),
    ok.

-spec delete_all(integer(), binary(), #context{}) -> ok.
delete_all(RscId, Sub, Context) ->
    z_db:q("delete from chatpage_message where rsc_id = $1 and sub = $2", [RscId, Sub], Context),
    ok.


install(Context) ->
    case z_db:table_exists(chatpage_message, Context) of
        false ->
            [] = z_db:q("
                    create table chatpage_message (
                        id bigserial not null,
                        rsc_id int not null,
                        sub character varying(20) default '',
                        user_id int,
                        message text,
                        uniqueid character varying(40),
                        created timestamp with time zone not null default now(),

                        primary key(id),
                        constraint fk_chatpage_message_rsc_id foreign key (rsc_id) references rsc(id)
                            on update cascade
                            on delete cascade,
                        constraint fk_chatpage_message_user_id foreign key (user_id) references rsc(id)
                            on update cascade
                            on delete cascade
                    )
                    ", Context),
            [] = z_db:q("CREATE INDEX fki_chatpage_message_rsc_id ON chatpage_message (rsc_id)", Context),
            [] = z_db:q("CREATE INDEX fki_chatpage_message_user_id ON chatpage_message (user_id)", Context),
            [] = z_db:q("CREATE INDEX chatpage_message_created_key ON chatpage_message (created)", Context),
            [] = z_db:q("CREATE INDEX chatpage_message_rsc_id_id_key ON chatpage_message (rsc_id, sub, id)", Context),
            ok;
        true ->
            ok
    end.

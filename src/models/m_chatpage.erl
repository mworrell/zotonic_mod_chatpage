%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2023 Marc Worrell
%% @doc Model for chats coupled to a resource.
%% @end

%% Copyright 2016-2023 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(m_chatpage).

-export([
    m_get/3,
    m_post/3,

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

-include_lib("zotonic_core/include/zotonic.hrl").

-define(PAGELEN, 50).

m_get([ <<"list">>, RscId, Sub | Rest ], _Msg, Context) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        Id ->
            case is_visible(Id, Context) of
                true ->
                    case list(Id, undefined, z_convert:to_binary(Sub), ?PAGELEN, Context) of
                        {ok, List} ->
                            {ok, {List, Rest}};
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {error, eacces}
            end
    end;
m_get([ <<"recent">>, RscId, Sub | Rest ], _Msg, Context) ->
    case is_visible(RscId, Context) of
        true ->
            case recent(RscId, z_convert:to_binary(Sub), Context) of
                {ok, Recent} ->
                    {ok, {Recent, Rest}};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, eacces}
    end;
m_get([ <<"archive">>, RscId, Sub ], _Msg, Context) ->
    case handle_archive(m_rsc:rid(RscId, Context), Sub, undefined, Context) of
        {ok, Result} ->
            {ok, {Result, []}};
        {error, _} = Error ->
            Error
    end;
m_get([ <<"archive">>, RscId, Sub, FromId | Rest ], _Msg, Context) ->
    case handle_archive(m_rsc:rid(RscId, Context), Sub, FromId, Context) of
        {ok, Result} ->
            {ok, {Result, Rest}};
        {error, _} = Error ->
            Error
    end.

m_post([ <<"message">>, RscId, Sub ], #{
            payload := #{
                <<"message">> := PayloadMessage,
                <<"uniqueid">> := UniqueId
            }
        }, Context) when is_binary(PayloadMessage) ->
    case m_rsc:rid(RscId, Context) of
        undefined ->
            {error, enoent};
        Id ->
            case is_visible(Id, Context) of
                true ->
                    UserId = z_acl:user(Context),
                    Message = prepare_message(Id, Sub, PayloadMessage, Context),
                    {ok, MsgId} = insert(Id, Sub, UserId, Message, UniqueId, Context),
                    post_message(Id, Sub, MsgId, Context);
                false ->
                    {error, eacces}
            end
    end;
m_post(Topic, #{ payload := Payload }, _Context) ->
    ?LOG_WARNING(#{
        in => zotonic_mod_chatpage,
        text => <<"Unknown post for chatpage">>,
        result => error,
        reason => payload,
        payload => Payload,
        topic => Topic
    }),
    ok.

prepare_message(ChatId, Sub, Message, Context) when is_binary(Message) ->
    case z_notifier:first({chatpage_message, ChatId, Sub, Message}, Context) of
        undefined -> z_html:escape_link(Message);
        {text, Msg} -> z_html:escape_link(Msg);
        {html, Msg} -> z_sanitize:html(Msg, Context)
    end.

post_message(ChatId, Sub, MsgId, Context) ->
    case m_chatpage:get(MsgId, Context) of
        {ok, Post} ->
            PageKey = make_pageid(ChatId, Sub),
            Topic = <<"chatpage/",PageKey/binary>>,
            Data = render_messages(PageKey, [Post], undefined, Context),
            Msg = #{
                <<"status">> => <<"ok">>,
                <<"result">> => Data
            },
            z_mqtt:publish(Topic, Msg, z_acl:sudo(Context));
        {error, _} = Error ->
            Error
    end.

render_messages(PageKey, Posts, MaybeHasMore, Context) ->
    Messages = [
        #{
            <<"id">> => maps:get(<<"id">>, Post),
            <<"uniqueid">> => maps:get(<<"uniqueid">>, Post),
            <<"html">> => render_post(Post, Context)
        }
        || Post <- Posts
    ],
    #{
        <<"page_id">> => PageKey,
        <<"hasmore">> => MaybeHasMore,
        <<"messages">> => Messages
    }.

handle_archive(ChatId, Sub, FromId, Context) ->
    {ok, List} = list(ChatId, Sub, FromId, ?PAGELEN+1, Context),
    HasMore = length(List) > ?PAGELEN,
    List1 = case HasMore of
                true -> lists:sublist(List, ?PAGELEN);
                false -> List
            end,
    PageKey = make_pageid(ChatId, Sub),
    Data = render_messages(PageKey, List1, HasMore, Context),
    {ok, Data}.

make_pageid(RscId, Sub) ->
    <<(z_convert:to_binary(RscId))/binary, $/, Sub/binary>>.

render_post(Post, Context) ->
    Vars = [
        {post, Post}
    ],
    {Html, _} = z_template:render_to_iolist("_chatpage_message.tpl", Vars, z_acl:sudo(Context)),
    iolist_to_binary(Html).

-spec is_visible(RscId, Context) -> boolean() when
    RscId :: m_rsc:resource_id(),
    Context :: z:context().
is_visible(RscId, Context) ->
    z_acl:rsc_visible(RscId, Context)
    andalso z_acl:is_allowed(use, mod_chatpage, Context).

-spec get(MsgId, Context) -> {ok, map()} | {error, enoent} when
    MsgId :: integer(),
    Context :: z:context().
get(MsgId, Context) ->
    z_db:qmap_row("
            select *
            from chatpage_message
            where id = $1
            ",
            [MsgId],
            Context).

-spec recent(RscId, Sub, Context) -> {ok, list(map())} | {error, enoent} when
    RscId :: m_rsc:resource_id(),
    Sub :: binary(),
    Context :: z:context().
recent(RscId, Sub, Context) ->
    z_db:qmap("
            select *
            from chatpage_message
            where rsc_id = $1
              and sub = $2
            order by id desc
            limit 1
            ",
            [RscId, Sub],
            Context).


-spec list(RscId, Sub, FromId, PageLen, Context) -> {ok, list(map())} | {error, enoent} when
    RscId :: m_rsc:resource_id(),
    Sub :: binary(),
    FromId :: integer(),
    PageLen :: pos_integer(),
    Context :: z:context().
list(RscId, Sub, undefined, PageLen, Context) ->
    z_db:qmap("
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


-spec insert(RscId, Sub, UserId, Message, Context) -> {ok, integer()} | {error, term()} when
    RscId :: m_rsc:resource_id(),
    Sub :: binary(),
    UserId :: m_rsc:resource_id() | undefined,
    Message :: binary(),
    Context :: z:context().
insert(RscId, Sub, UserId, Message, Context) ->
    insert(RscId, Sub, UserId, Message, undefined, Context).

-spec insert(RscId, Sub, UserId, Message, Unique, Context) -> {ok, integer()} | {error, term()} when
    RscId :: m_rsc:resource_id(),
    Sub :: binary(),
    UserId :: m_rsc:resource_id() | undefined,
    Message :: binary(),
    Unique :: binary(),
    Context :: z:context().
insert(RscId, Sub, UserId, Message, Unique, Context) ->
    Props = #{
        <<"rsc_id">> => RscId,
        <<"sub">> => Sub,
        <<"message">> => Message,
        <<"uniqueid">> => Unique
    },
    Props1 = if
        UserId =:= undefined ->
            Props;
        is_integer(UserId) ->
            Props#{
                <<"user_id">> => UserId
            }
    end,
    z_db:insert(chatpage_message, Props1, Context).

%% @doc Delete a specific message in a chat
-spec delete(RscId, MsgId, Context) -> ok when
    RscId :: m_rsc:resource_id(),
    MsgId :: integer(),
    Context :: z:context().
delete(RscId, MsgId, Context) ->
    z_db:q("delete from chatpage_message
            where rsc_id = $1
              and id = $2",
           [RscId, MsgId], Context),
    ok.

%% @doc Delete all messages in a chat
-spec delete_all(RscId, Context) -> ok when
    RscId :: m_rsc:resource_id(),
    Context :: z:context().
delete_all(RscId, Context) ->
    z_db:q("delete from chatpage_message where rsc_id = $1", [RscId], Context),
    ok.

-spec delete_all(RscId, Sub, Context) -> ok when
    RscId :: m_rsc:resource_id(),
    Sub :: binary(),
    Context :: z:context().
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

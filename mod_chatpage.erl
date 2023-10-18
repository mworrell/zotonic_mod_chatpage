-module(mod_chatpage).

-author('Marc Worrell <marc@worrell.nl>').

-mod_title("Chat on a page").
-mod_description("Chat coupled to pages, with archiving and presence.").
-mod_schema(2).
-mod_depends([mod_mqtt]).

-include("zotonic.hrl").
-include_lib("emqtt/include/emqtt.hrl").

-define(POSTS_PER_PAGE, 50).

-export([
    'mqtt:~site/chatpage/#'/3,
    observe_acl_is_allowed/2,
    manage_schema/2
    ]).


%% @doc Check if the user can publish/subscribe to the chatpage topics
-spec observe_acl_is_allowed(#acl_is_allowed{}, #context{}) -> boolean() | undefined.
observe_acl_is_allowed(
        #acl_is_allowed{
            action=subscribe, 
            object=#acl_mqtt{words=[<<"site">>, _, <<"chatpage">> | SubTopic ]}},
        Context) ->
    z_acl:is_allowed(use, mod_chatpage, Context) andalso can_subscribe(SubTopic, Context);
observe_acl_is_allowed(
        #acl_is_allowed{
            action=publish, 
            object=#acl_mqtt{words=[<<"site">>, _, <<"chatpage">> | SubTopic ]}},
        Context) ->
    z_acl:is_allowed(use, mod_chatpage, Context) andalso can_publish(SubTopic, Context);
observe_acl_is_allowed(_, _Context) ->
    undefined.


can_subscribe([RscId, Sub], Context) -> can_access(RscId, Sub, Context);
can_subscribe([RscId, Sub, <<"presence">>], Context) -> can_access(RscId, Sub, Context);
can_subscribe(_, _Context) -> false.

can_publish([<<"presence">>, <<"html">>], _Context) -> true;
can_publish([RscId, Sub, <<"post">>], Context) -> can_access(RscId, Sub, Context);
can_publish([RscId, Sub, <<"presence">>], Context) -> can_access(RscId, Sub, Context);
can_publish([RscId, Sub, <<"archive">> | _], Context) -> can_access(RscId, Sub, Context);
can_publish(_, _Context) -> false.

can_access(RscId, <<"default">>, Context) -> z_acl:rsc_visible(RscId, Context);
can_access(RscId, <<"editors">>, Context) -> z_acl:rsc_editable(RscId, Context);
can_access(RscId, <<"mod_", _/binary>> = Module, Context) ->
    try
        ModuleAtom = erlang:list_to_existing_atom(binary_to_list(Module)), 
        z_acl:rsc_visible(RscId, Context)
        andalso z_acl:is_allowed(use, ModuleAtom, Context)
    catch
        _:_ -> false
    end;
can_access(_RscId, _Sub, _Context) -> undefined.


% Pages subscribe to ~site/chatpage/1234 for new messages

% Publish a new message to ~site/chatpage/1234/post
% On a message:
% 1. Filter (acl checks etc)
% 2. Publish to ~site/chatpage/1234
% 3. Store in database

% Publish to ~site/chatpage/1234/archive/<beforeid> to receive archived messages
% Archived messages are published to ~pagesession/chatpage/1234

% Publish presence on ~site/chatpage/1234/presence (filter present message)
% Listen on ~site/chatpage/1234/presence for presence changes
% state is one of: online, typing, away

% If a client receives presence from a new user, then that client publishes its presence.
% In this way we don't need to track who is online, the clients do that.

'mqtt:~site/chatpage/#'(Message, _Pid, Context) ->
    % ~site/chatpage/1234/default/post
    % ~site/chatpage/1234/default/presence (state)
    % ~site/chatpage/1234/default/archive/<beforeid> (state)
    % lager:info("mqtt:~~site/chatpage/# received: ~p", [{Message, Pid, z_context:site(Context)}]),
    Words = binary:split(Message#mqtt_msg.topic, <<"/">>, [global]),
    handle_mqtt(Words, Message, Context).


handle_mqtt([<<"site">>, _Site, <<"chatpage">>, <<"presence">>, <<"html">>], Message, Context) ->
    {ok, Data} = z_mqtt:payload_data(Message),
    ReplyTopic =  proplists:get_value(<<"reply_topic">>, Data),
    UserId = m_rsc:rid(proplists:get_value(<<"user_id">>, Data), Context),
    ClientId = proplists:get_value(<<"client_id">>, Data),
    ContextUser = set_payload_user(Message, Context),
    Html = render_presence(UserId, ClientId, ContextUser),
    z_mqtt:publish(ReplyTopic, [
            {user_id, UserId},
            {client_id, ClientId},
            {html, Html}
        ], ContextUser);
handle_mqtt([<<"site">>, _Site, <<"chatpage">>, Page, Sub, <<"post">>], Message, Context) ->
    {ok, UserId} = z_mqtt:payload_user(Message),
    {ok, Payload} = z_mqtt:payload_data(Message),
    ContextUser = set_payload_user(Message, Context),
    handle_post(m_rsc:rid(Page, Context), Sub, UserId, Payload, ContextUser);
handle_mqtt([<<"site">>, _Site, <<"chatpage">>, Page, Sub, <<"archive">>], Message, Context) ->
    handle_archive(m_rsc:rid(Page, Context), Sub, undefined, Message, Context);
handle_mqtt([<<"site">>, _Site, <<"chatpage">>, Page, Sub, <<"archive">>, FromId], Message, Context) ->
    handle_archive(m_rsc:rid(Page, Context), Sub, binary_to_integer(FromId), Message, Context);
handle_mqtt(_Topic, _Message, _Context) ->
    ok.


handle_archive(ChatId, Sub, FromId, Message, Context) ->
    {ok, Payload} = z_mqtt:payload_data(Message),
    ReplyTopic =  proplists:get_value(<<"reply_topic">>, Payload),
    List = m_chatpage:list(ChatId, Sub, FromId, ?POSTS_PER_PAGE+1, Context),
    HasMore = length(List) > ?POSTS_PER_PAGE,
    List1 = case HasMore of
                true -> lists:sublist(List, ?POSTS_PER_PAGE);
                false -> List
            end,
    PageKey = make_pageid(ChatId, Sub),
    post_messages(ReplyTopic, PageKey, List1, HasMore, Context).

handle_post(ChatId, Sub, UserId, Payload, Context) when is_list(Payload) ->
    Message = prepare_message(ChatId, Sub, proplists:get_value(<<"message">>, Payload), Context),
    UniqueId = proplists:get_value(<<"uniqueid">>, Payload),
    {ok, MsgId} = m_chatpage:insert(ChatId, Sub, UserId, Message, UniqueId, Context),
    post_message(ChatId, Sub, MsgId, Context);
handle_post(_PageId, _Sub, _UserId, Payload, _Context) ->
    lager:warning("Uknown payload for chatpage post: ~p", [Payload]),
    ok.

prepare_message(ChatId, Sub, Message, Context) when is_binary(Message) ->
    case z_notifier:first({chatpage_message, ChatId, Sub, Message}, Context) of
        undefined -> z_html:escape_link(Message);
        {text, Msg} -> z_html:escape_link(Msg);
        {html, Msg} -> z_sanitize:html(Msg, Context)
    end.

post_message(ChatId, Sub, MsgId, Context) ->
    PageKey = make_pageid(ChatId, Sub),
    Post = m_chatpage:get(MsgId, Context),
    Topic = <<"~site/chatpage/",PageKey/binary>>,
    post_messages(Topic, PageKey, [Post], undefined, Context).

post_messages(Topic, PageKey, Posts, MaybeHasMore, Context) ->
    Data = [
        {page_id, PageKey},
        {hasmore, MaybeHasMore},
        {messages, [
            [
                {id, proplists:get_value(id, Post)},
                {uniqueid, proplists:get_value(uniqueid, Post)},
                {html, render_post(Post, Context)}
            ]
            || Post <- Posts
        ]}
    ],
    z_mqtt:publish(Topic, Data, z_acl:sudo(Context)).

render_post(Post, Context) ->
    Vars = [
        {post, Post}
    ],
    {Html, _} = z_template:render_to_iolist("_chatpage_message.tpl", Vars, z_acl:sudo(Context)),
    iolist_to_binary(Html).

render_presence(UserId, ClientId, Context) ->
    Vars = [
        {user_id, UserId},
        {client_id, ClientId}
    ],
    {Html, _} = z_template:render_to_iolist("_chatpage_presence.tpl", Vars, Context),
    iolist_to_binary(Html).

make_pageid(RscId, Sub) ->
    <<(z_convert:to_binary(RscId))/binary, $/, Sub/binary>>.

set_payload_user(Msg, Context) ->
    case z_mqtt:payload_user(Msg) of
        {ok, undefined} -> z_acl:anondo(Context);
        {ok, UserId} -> z_acl:logon(UserId, Context)
    end.

manage_schema(_Version, Context) ->
    m_chatpage:install(Context),
    #datamodel{
        categories=
            [
                {chatpage, undefined, [
                    {title, {trans, [{en, <<"Chat Page">>}, {nl, <<"Chat Pagina">>}]}}
                ]}
            ],
        resources=
            [
                {page_admin_chatpage, chatpage, [
                       {title, {trans, [{en, <<"Editor’s Chat"/utf8>>}, {nl, <<"Redactie Chat">>}]}}
                ]}
            ]
    }.

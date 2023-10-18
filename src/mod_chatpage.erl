%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2016-2023 Marc Worrell
%% @doc Chats for in the browser, coupled to a resource.
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

-module(mod_chatpage).

-author('Marc Worrell <marc@worrell.nl>').

-mod_title("Chat on a page").
-mod_description("Chat coupled to pages, with archiving and presence.").
-mod_schema(2).
-mod_depends([mod_mqtt]).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    observe_acl_is_allowed/2,
    manage_schema/2
    ]).


%% @doc Check if the user can publish/subscribe to the chatpage topics
-spec observe_acl_is_allowed(#acl_is_allowed{}, Context) -> boolean() | undefined when
    Context :: z:context().
observe_acl_is_allowed(
        #acl_is_allowed{
            action=subscribe,
            object=#acl_mqtt{topic=[<<"chatpage">> | SubTopic ]}},
        Context) ->
    z_acl:is_allowed(use, mod_chatpage, Context) andalso can_subscribe(SubTopic, Context);
observe_acl_is_allowed(
        #acl_is_allowed{
            action=publish,
            object=#acl_mqtt{topic=[<<"chatpage">> | SubTopic ]}},
        Context) ->
    z_acl:is_allowed(use, mod_chatpage, Context) andalso can_publish(SubTopic, Context);
observe_acl_is_allowed(_, _Context) ->
    undefined.


can_subscribe([RscId, Sub], Context) -> can_access(RscId, Sub, Context);
can_subscribe([RscId, Sub, <<"presence">>], Context) -> can_access(RscId, Sub, Context);
can_subscribe(_, _Context) -> false.

% can_publish([<<"presence">>, <<"html">>], _Context) -> true;
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

manage_schema(_Version, Context) ->
    m_chatpage:install(Context),
    #datamodel{
        categories=
            [
                {chatpage, undefined, [
                    {title, #trans{
                        tr = [{en, <<"Chat Page">>}, {nl, <<"Chat Pagina">>}]
                    }}
                ]}
            ],
        resources=
            [
                {page_admin_chatpage, chatpage, [
                       {title, #trans{
                            tr = [{en, <<"Editor’s Chat"/utf8>>}, {nl, <<"Redactie Chat">>}]
                        }}
                ]}
            ]
    }.

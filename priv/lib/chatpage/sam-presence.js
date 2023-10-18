// presence

/**
 * @author Marc Worrell <marc@worrell.nl>
 * @copyright 2016-2023 Marc Worrell
 * @doc Chat using Cotonic MQTT - Presence model
 *
 * Copyright 2016-2023 Marc Worrell
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

(function($) {

    const DOM_ELEMENT_ID = "chatpage-presence";

    const IDLE_TIMEOUT   = 60;
    const AWAY_TIMEOUT   = 300;
    const TYPING_TIMEOUT = 10;

    const PUBLISH_PERIOD = 7;
    const GONE_PERIOD    = 20;
    const CLEANUP_PERIOD = 60;

    const STATUS_GONE    = 0;
    const STATUS_AWAY    = 1;
    const STATUS_IDLE    = 2;
    const STATUS_PRESENT = 3;
    const STATUS_TYPING  = 4;

    const HTML_PLACEHOLDER = "...";

    ////////////////////////////////////////////////////////////////////////////////
    // Model
    //
    var model = {
        client_id: undefined,
        user_id: undefined,
        page_id: undefined,
        sub_id: undefined,
        is_sub_acked: false,
        is_sub_html_acked: false,
        status: STATUS_PRESENT,
        active: 0,
        presences: [
            // {
            //     user_id: ...,
            //     client_id: ...,
            //     last_seen: ...,
            //     status: 2
            // }
        ],
        html: [
        ]
    };

    function unique_id() {
        let t = (new Date()).getTime() + "-";
        const cs = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
        for (let i=0; i < 20; i++) {
            t += cs.charAt(Math.floor(Math.random() * cs.length));
        }
        return t;
    }

    model.propose = function(data) {
        const now = Math.round(Date.now() / 1000);
        let publish_presence = false;

        // Modifiy model with data, if acceptable
        data = data || {} ;

        if (data.new_page_id !== undefined && data.new_page_id !== model.page_id) {
            if (model.page_id) {
                cotonic.broker.publish(
                    "bridge/origin/chatpage/"+model.page_id+"/presence",
                    {
                        client_id: model.client_id,
                        page_id: model.page_id,
                        status: STATUS_GONE
                    },
                    { qos: 1 });
            }
            if (model.sub_id) {
                cotonic.broker.unsubscribe(model.sub_id);
            }
            model.page_id = data.new_page_id;
            model.sub_id = undefined;
            model.is_sub_acked = false;
            model.presences = [];
            model.active = now;
        }

        if (data.is_subscribed === true && state.unsubscribed(model)) {
            model.sub_id = cotonic.broker.subscribe(
                                "bridge/origin/chatpage/"+model.page_id+"/presence",
                                function(msg) {
                                    actions.presence(msg);
                                },
                                { qos: 1 });

            publish_presence = true;
        }

        for (let ph in model.html) {
            if (ph.html == HTML_PLACEHOLDER) {
                const p_user_id = ph.user_id;
                const p_client_id = ph.client_id;

                cotonic.broker.call(
                    "bridge/origin/model/template/get/render/_chatpage_presence.tpl",
                    {
                        user_id: p_user_id,
                        client_id: p_client_id
                    },
                    { qos: 1 })
                .then(function(msg) {
                    const data = {
                        client_id: p_client_id,
                        user_id: p_user_id,
                        html: msg.payload.result
                    }
                    actions.presence_html(data);
                });
            }
        }

        if (data.presence !== undefined && data.presence.page_id == model.page_id) {
            let i;

            for (i=0; i<model.presences.length; i++) {
                if (model.presences[i].client_id == data.presence.client_id) {
                    break;
                }
            }
            if (i == model.presences.length) {
                var p = {
                    user_id: data.presence.user_id,
                    client_id: data.presence.client_id,
                    last_seen: now,
                    status: data.presence.status
                };
                model.presences.push(p);
                if (!(p.client_id in model.html)) {
                    model.html[p.client_id] = {
                        user_id: p.user_id,
                        client_id: p.client_id,
                        html: HTML_PLACEHOLDER
                    };
                }
                if (data.presence.client_id != model.client_id) {
                    publish_presence = true;
                }
            } else if (data.presence.status == STATUS_GONE && data.presence.client_id != model.client_id) {
                model.presences.splice(i, 1);
            } else {
                model.presences[i].user_id = data.presence.user_id;
                model.presences[i].status = data.presence.status;
                model.presences[i].last_seen = now;
            }
            model.presences = model.presences.filter(function(p) {
                return p.client_id == model.client_id || (now - p.last_seen) < CLEANUP_PERIOD;
            });

            if (model.html[data.presence.client_id].html == HTML_PLACEHOLDER) {
                const p_client_id = data.presence.client_id;
                const p_user_id = data.presence.user_id;
                cotonic.broker.call(
                    "bridge/origin/model/template/get/render/_chatpage_presence.tpl",
                    {
                        user_id: p_user_id,
                        client_id: p_client_id
                    },
                    { qos: 1 })
                .then(function(msg) {
                    const data = {
                        client_id: p_client_id,
                        user_id: p_user_id,
                        html: msg.payload.result
                    }
                    actions.presence_html(data);
                });
            }

            for (i=0; i<model.presences.length; i++) {
                if (model.presences[i].client_id == model.client_id) {
                    if (now - model.presences[i].last_seen > GONE_PERIOD) {
                        model.presences[i].status = STATUS_GONE;
                    }
                }
            }
            model.presences.sort(
                    function(a,b) {
                        if (a.status < b.status) return 1;
                        if (a.status > b.status) return -1;
                        return 0;
                    });
        }

        if (data.html !== undefined) {
            model.html[data.client_id] = {
                user_id: data.user_id,
                html: data.html
            };
        }

        if (data.ping === true) {
            const inactive_period = now - model.active;
            const old_status = model.status;

            if (inactive_period > AWAY_TIMEOUT) {
                model.status = STATUS_AWAY;
            } else if (inactive_period > IDLE_TIMEOUT) {
                model.status = STATUS_IDLE;
            } else if (model.status == STATUS_TYPING && inactive_period > TYPING_TIMEOUT) {
                model.status = STATUS_PRESENT;
            }
            if (model.status != old_status) {
                publish_presence = true;
            }
            setTimeout(function() { actions.ping({}); }, 1000);
        }

        if (data.status !== undefined && data.status >= STATUS_PRESENT) {
            model.active = now;
        }
        if (data.status !== undefined && data.status != model.status) {
            if (data.status == STATUS_IDLE) {
                if (model.status > data.status) {
                    model.status = STATUS_IDLE;
                    publish_presence = true;
                }
            }
            else if (model.status != STATUS_TYPING) {
                model.status = data.status;
                publish_presence = true;
            }
        }

        if (model.page_id !== undefined && (publish_presence || (now - model.last_publish) > PUBLISH_PERIOD)) {
            model.last_publish = now;
            cotonic.broker.publish(
                "bridge/origin/chatpage/"+model.page_id+"/presence",
                {
                    user_id: model.user_id,
                    client_id: model.client_id,
                    page_id: model.page_id,
                    status: model.status
                },
                { qos: 1 });
        }

        // Render the new view
        state.render(model);
    };


    ////////////////////////////////////////////////////////////////////////////////
    // View
    //
    var view = {} ;

    // Initial State
    view.init = function(model) {
        model.client_id = unique_id();
        model.active = Date.now() / 1000;
        model.user_id = undefined;

        cotonic.broker.subscribe(
            "model/auth/event/auth",
            function(msg) {
                model.user_id = msg.payload.user_id;
            });

        cotonic.broker.subscribe(
            "chatpage/selectroom",
            function(msg) { actions.selectroom(msg.payload || msg); });

        var stateRepresentation = document.getElementById(DOM_ELEMENT_ID);
        if (stateRepresentation.dataset.pageId) {
            model.page_id = stateRepresentation.dataset.pageId + "/" +
                            (stateRepresentation.dataset.chatName || "default");
        }
        $(window)
            .blur(function() { actions.idle(); })
            .focus(function() { actions.active(); })
            .scroll(function() { actions.active(); })
            .on("beforeunload",function() { actions.gone(); });
        $(document).on("mousemove keyup touchstart", function(){
            actions.active();
        });
        setTimeout(function() { actions.ping({}); }, 1000);
        return view.ready(model) ;
    } ;

    // State representation of the ready state
    view.ready = function(model) {
        var output = state.representation(model);
        return output ;
    };


    // display the state representation
    view.display = function(representation) {
        var stateRepresentation = document.getElementById(DOM_ELEMENT_ID);
        stateRepresentation.innerHTML = representation;
    };

    ////////////////////////////////////////////////////////////////////////////////
    // State
    //
    var state =  { view: view };

    model.state = state ;

    // Derive the state representation as a function of the systen
    // control state
    state.representation = function(model) {
        var representation = "";
        var users = [];

        for (var k in model.presences) {
            var p = model.presences[k];
            if (p.status != STATUS_GONE) {
                if (!p.user_id || !(p.user_id in users)) {
                    var c = 'label-default';
                    switch (model.presences[k].status) {
                        case STATUS_AWAY:
                            break;
                        case STATUS_IDLE:
                            c = 'label-info';
                            break;
                        case STATUS_PRESENT:
                        case STATUS_TYPING:
                            c = 'label-success';
                            break;
                    }
                    representation += "<div class='label "+c+"'>"+
                                      (model.html[model.presences[k].client_id].html || HTML_PLACEHOLDER)+
                                      "</div>";

                    if (p.user_id) {
                        users[p.user_id] = true;
                    }
                }
            }
        }
        state.view.display(representation) ;
    } ;

    // Derive the current state of the system
    state.idle = function(model) {
       return model.page_id === undefined;
    };

    state.unsubscribed = function(model) {
       return model.page_id !== undefined && model.sub_id === undefined;
    };

    state.subscribing = function(model) {
       return model.page_id !== undefined && model.sub_id !== undefined && !model.is_sub_acked;
    };

    state.subscribed = function(model) {
       return model.page_id !== undefined && model.sub_id !== undefined && model.is_sub_acked;
    };

    // Next action predicate, derives whether
    // the system is in a (control) state where
    // an action needs to be invoked

    state.nextAction = function(model) {
        if (state.unsubscribed(model)) {
            actions.subscribe({});
        }
    };

    state.render = function(model) {
        state.representation(model);
        state.nextAction(model) ;
    };


    ////////////////////////////////////////////////////////////////////////////////
    // Actions
    //

    var actions = {} ;

    actions.selectroom = function(data) {
        data = {
            new_page_id: data.page_id
        };
        model.propose(data);
    };

    actions.subscribe = function(data) {
        data = {
            is_subscribed: true
        };
        model.propose(data);
    };

    actions.ping = function(_data) {
        model.propose({ ping: true });
    };

    actions.idle = function(_data) {
        model.propose({ status: STATUS_IDLE });
    };

    actions.active = function(_data) {
        model.propose({ status: STATUS_PRESENT });
    };

    actions.gone = function(_data) {
        model.propose({ status: STATUS_GONE });
    };

    actions.presence = function(msg) {
        data = {
            presence: {
                user_id: msg.payload.user_id,
                page_id: msg.payload.page_id,
                client_id: msg.payload.client_id,
                status: msg.payload.status
            }
        };
        model.propose(data) ;
    };

    actions.presence_html = function(msg) {
        data = {
            user_id: msg.user_id,
            client_id: msg.client_id,
            html: msg.html
        };
        model.propose(data);
    };

    ////////////////////////////////////////////////////////////////////////////////
    // Display initial state
    //

    if (document.getElementById(DOM_ELEMENT_ID)) {
        view.display(view.init(model)) ;
        state.nextAction(model);
    }
})(jQuery);


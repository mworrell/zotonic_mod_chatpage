/**
 * @author Marc Worrell <marc@worrell.nl>
 * @copyright 2016-2023 Marc Worrell
 * @doc Chat using Cotonic MQTT
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

    const DOM_ELEMENT_ID = "chatpage-chat";
    const CALL_TIMEOUT = 60000;

    ////////////////////////////////////////////////////////////////////////////////
    // Model
    //
    var model = {
        page_id: undefined,
        sub_id: undefined,
        is_loaded: false,
        is_loaded_all: false,
        messages: [
            // {
            //     msg_id: 2,
            //     uniqueid: "...",
            //     html: "..."
            // }
        ],
        formsubmits: [
            // {
            //     page_id: "...",
            //     uniqueid: "...",
            //     html: "..."
            // }
        ]
    };

    model.propose = function(data) {
        // Modifiy model with data, if acceptable
        data = data || {} ;

        if (data.new_page_id !== undefined && data.new_page_id !== model.page_id) {
            if (model.sub_id) {
                cotonic.broker.unsubscribe(model.sub_id);
            }
            model.page_id = data.new_page_id;
            model.sub_id = undefined;
            model.is_loaded = false;
            model.is_loading = false;
            model.is_loaded_all = false;
            model.messages = [];
            model.formsubmits = [];
        }

        if (model.page_id !== undefined && data.is_subscribed !== undefined && state.unsubscribed(model)) {
            model.sub_id = cotonic.broker.subscribe(
                                "bridge/origin/chatpage/"+model.page_id,
                                function(msg) {
                                    actions.messages(msg.payload);
                                },
                                { qos: 1 });
        }

        if (state.subscribed(model) && !model.is_loaded && !model.is_loading) {
            model.is_loading = true;
            cotonic.broker.call(
                "bridge/origin/model/chatpage/get/archive/"+model.page_id,
                {},
                { qos: 1, timeout: CALL_TIMEOUT })
            .then(function(msg) {
                model.is_loading = false;
                actions.messages(msg.payload);
            });
        }

        if (data.messages !== undefined && data.page_id == model.page_id) {
            let max_id;

            model.is_loaded = true;
            if (data.hasmore === false) {
                model.is_loaded_all = true;
            }
            if (model.messages.length > 0) {
                max_id = model.messages[model.messages.length-1].id;
            }

            // Merge the messages from data with state
            data.messages.sort(
                    function(a,b) {
                        if (a.id > b.id) return 1;
                        if (a.id < b.id) return -1;
                        return 0;
                    });
            let merged = [];
            let sn = 0;
            let dn = 0;

            while (sn < model.messages.length && dn < data.messages.length) {
                const a = model.messages[sn];
                const b = data.messages[dn];
                if (a.id < b.id) {
                    merged.push(a);
                    sn++;
                } else if (a.id > b.id) {
                    merged.push(b);
                    dn++;
                } else {
                    merged.push(a);
                    sn++;
                    dn++;
                }
            }
            while (sn < model.messages.length) {
                merged.push(model.messages[sn++]);
            }
            while (dn < data.messages.length) {
                merged.push(data.messages[dn++]);
            }
            model.messages = merged;

            // Remove messages with matching uniqueid from formsubmits
            let uniqueids = [];
            len = model.messages.length;
            for (sn = 0; sn < len; sn++) {
                uniqueids[model.messages[sn].uniqueid] = true;
            }
            model.formsubmits = model.formsubmits.filter(
                function(sub) {
                    return !(sub.uniqueid in uniqueids);
                });

            if (model.messages.length > 0) {
                model.is_appended = (max_id < model.messages[model.messages.length-1].id);
            } else {
                model.is_appended = true;
            }
        }

        if (data.is_loadmore === true && data.page_id == model.page_id) {
            if (model.is_loaded && !model.is_loaded_all) {
                let topic = "bridge/origin/chatpage/archive/"+model.page_id;
                let min_id = 0;
                for (let i=0; i<model.messages.length; i++) {
                    if (min_id) min_id = Math.min(model.messages[i].id, min_id);
                    else min_id = model.messages[i].id;
                }
                if (min_id) {
                    topic += "/"+min_id;
                }
                cotonic.broker.call(
                    topic,
                    {},
                    { qos: 1, timeout: CALL_TIMEOUT })
                .then(function(msg) {
                    actions.messages(msg.payload);
                });
            }
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
        cotonic.broker.subscribe(
            "chatpage/selectroom",
            function(msg) {
                actions.selectroom(msg.payload || msg);
            });
        cotonic.broker.subscribe(
            "chatpage/formsubmit",
            function(msg) {
                actions.formsubmit(msg.payload || msg);
            });

        var stateRepresentation = document.getElementById(DOM_ELEMENT_ID);
        if (stateRepresentation.dataset.pageId) {
            model.page_id = stateRepresentation.dataset.pageId + "/" +
                            (stateRepresentation.dataset.chatName || "default");
        }
        $('#'+DOM_ELEMENT_ID).on('click', ".chatpage-load-more", function(e) {
            e.preventDefault();
            $(this).addClass("loading");
            actions.loadmore({ page_id: model.page_id });
        });
        return view.ready(model) ;
    } ;

    // State representation of the ready state
    view.ready = function(model) {
        const output = state.representation(model);
        return output ;
    };


    // display the state representation
    view.display = function(representation) {
        let stateRepresentation = document.getElementById(DOM_ELEMENT_ID);
        const prevScrollHeight = stateRepresentation.scrollHeight;
        const prevScrollTop = stateRepresentation.scrollTop;

        stateRepresentation.innerHTML = representation;

        // Correct scrollbar
        if (model.is_appended) {
            stateRepresentation.scrollTop = stateRepresentation.scrollHeight;
        } else {
            const newHeight = $(stateRepresentation).height();
            const newScrollHeight = stateRepresentation.scrollHeight;
            if (newScrollHeight > newHeight && newScrollHeight > prevScrollHeight) {
                stateRepresentation.scrollTop = prevScrollTop + (newScrollHeight - prevScrollHeight);
            }
        }
    };

    ////////////////////////////////////////////////////////////////////////////////
    // State
    //
    var state =  { view: view };

    model.state = state ;

    // Derive the state representation as a function of the systen
    // control state
    state.representation = function(model) {
        let representation = "";
        for (let k in model.messages) {
            representation += model.messages[k].html;
        }
        if (!model.is_loaded_all && state.loaded(model)) {
            representation = $('#chatpage-load-more').html() + representation;
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

    state.subscribed = function(model) {
       return model.page_id !== undefined && model.sub_id !== undefined;
    };

    state.loaded = function(model) {
       return state.subscribed(model) && model.is_loaded;
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
        data = { new_page_id: data.page_id };
        model.propose(data);
    };

    actions.subscribe = function(data) {
        data = { is_subscribed: true };
        model.propose(data);
    };

    actions.subscribe_ack = function(data) {
        data = { received_subscribe_ack: data.page_id };
        model.propose(data);
    };

    actions.formsubmit = function(data) {
        // data.item = {name: data.name, description: data.description, id: data.id || null} ;
        // model.propose(data) ;
    };

    actions.messages = function(data) {
        if (data.status == "ok") {
            data = {
                page_id: data.result.page_id,
                messages: data.result.messages,
                hasmore: data.result.hasmore
            };
            model.propose(data) ;
        }
    };

    actions.loadmore = function(data) {
        data = {
            page_id: data.page_id,
            is_loadmore: true
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

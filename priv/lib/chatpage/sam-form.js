/**
 * @author Marc Worrell <marc@worrell.nl>
 * @copyright 2016-2023 Marc Worrell
 * @doc Form for chatpage using Cotonic MQTT
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

    const DOM_ELEMENT_ID = "chatpage-form";

    ////////////////////////////////////////////////////////////////////////////////
    // Model
    //
    let model = {
        page_id: undefined,
        height: undefined
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
        // Modifiy model with data, if acceptable
        data = data || {} ;

        if (data.new_page_id !== undefined && data.new_page_id !== model.page_id) {
            model.page_id = data.new_page_id;
        }

        if (model.page_id !== undefined && data.message !== undefined) {
            const message = data.message.trim();
            if (message.length > 0) {
                const post = {
                    page_id: model.page_id,
                    message: message,
                    uniqueid: unique_id()
                };
                cotonic.broker.publish(
                    "chatpage/formsubmit",
                    post);
                cotonic.broker.publish(
                    "bridge/origin/model/chatpage/post/message/"+model.page_id,
                    post,
                    { qos: 2 });

                $("#"+DOM_ELEMENT_ID+" form textarea")
                    .val("")
                    .height(model.height);
            }
        }

        // Render the new view
        state.render(model) ;
    };


    ////////////////////////////////////////////////////////////////////////////////
    // View
    //
    let view = {} ;

    // Initial State
    view.init = function(model) {
        cotonic.broker.subscribe(
            "chatpage/selectroom",
            function(msg) { actions.selectroom(msg.payload || msg); });

        $("#"+DOM_ELEMENT_ID+" form").on("submit", function(e) {
            e.preventDefault();
            data = {
                message: $(this).find("textarea").val()
            };
            actions.postmessage(data);
        });

        var $textarea =  $("#"+DOM_ELEMENT_ID+" form textarea");
        $textarea.on('keypress', function(e) {
            if (e.which == 13 && !e.shiftKey && !e.ctrlKey && !e.altKey) {
                e.preventDefault();
                $(this).closest('form').submit();
            }
        });
        model.height = $textarea.height();


        var stateRepresentation = document.getElementById(DOM_ELEMENT_ID);
        if (stateRepresentation.dataset.pageId) {
            model.page_id = stateRepresentation.dataset.pageId + "/" +
                            (stateRepresentation.dataset.chatName || "default");
        }
        return view.ready(model) ;
    } ;

    // State representation of the ready state
    view.ready = function(model) {
        return "";
    };


    // display the state representation
    view.display = function(representation) {
        // var stateRepresentation = document.getElementById(DOM_ELEMENT_ID);
        // stateRepresentation.innerHTML = representation;
    };

    // Display initial state
    if (document.getElementById(DOM_ELEMENT_ID)) {
        view.display(view.init(model));
    }


    ////////////////////////////////////////////////////////////////////////////////
    // State
    //
    let state =  { view: view };

    model.state = state ;

    // Derive thee state representation as a function of the systen
    // control state
    state.representation = function(model) {
        // state.view.display(representation) ;
    } ;

    // Derive the current state of the system
    state.disabled = function(model) {
       return model.page_id === undefined;
    };

    // Next action predicate, derives whether
    // the system is in a (control) state where
    // an action needs to be invoked

    state.nextAction = function(model) {
    };

    state.render = function(model) {
        state.representation(model);
        state.nextAction(model) ;
    } ;


    ////////////////////////////////////////////////////////////////////////////////
    // Actions
    //

    let actions = {} ;

    actions.selectroom = function(data) {
        data = { new_page_id: data.page_id };
        model.propose(data);
    };

    actions.postmessage = function(data) {
        data = {
            message: data.message
        };
        model.propose(data);
    };

})(jQuery);

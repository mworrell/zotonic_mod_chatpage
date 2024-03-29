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

    const DOM_ELEMENT_ID = "chatpage-rooms";

    ////////////////////////////////////////////////////////////////////////////////
    // Model
    //
    var model = {
        page_id: undefined
    };

    model.propose = function(data) {
        if (data.new_page_id !== undefined && data.new_page_id !== model.page_id) {
            model.page_id = data.new_page_id;
            cotonic.broker.publish(
                "chatpage/selectroom",
                { page_id: model.page_id },
                { qos: 1 });
            state.render(model) ;
        }
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
            },
            { qos: 1 });

        $("#"+DOM_ELEMENT_ID+" select").on("click change", function(e) {
            e.preventDefault();
            data = {
                page_id: $(this).val()
            };
            actions.selectroom(data);
        });

        const stateRepresentation = document.getElementById(DOM_ELEMENT_ID);
        if (stateRepresentation.dataset.pageId) {
            model.page_id = stateRepresentation.dataset.pageId + "/" +
                            (stateRepresentation.dataset.chatName || "default");
        }
        return view.ready(model) ;
    };

    // State representation of the ready state
    view.ready = function(model) {
        return "";
    };


    // display the state representation
    view.display = function(representation) {
        if (model.page_id && $('#'+DOM_ELEMENT_ID+" select").val() != model.page_id) {
            $('#'+DOM_ELEMENT_ID+" select").val(model.page_id);
        }
    };

    // Display initial state
    if (document.getElementById(DOM_ELEMENT_ID)) {
        view.display(view.init(model));
    }


    ////////////////////////////////////////////////////////////////////////////////
    // State
    //
    var state =  { view: view };

    model.state = state ;

    // Derive thee state representation as a function of the systen
    // control state
    state.representation = function(model) {
        const representation = "";
        state.view.display(representation) ;
    };

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

    var actions = {} ;

    actions.selectroom = function(data) {
        const parts = (''+data.page_id).split('/');
        if (parts.length == 2) {
            data = { new_page_id: data.page_id };
        } else {
            data = { new_page_id: data.page_id + "/default" };
        }
        model.propose(data);
    };

})(jQuery);

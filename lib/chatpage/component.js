/**
 * @author Marc Worrell <marc@worrell.nl>
 * @copyright 2016 Marc Worrell
 * @doc Chat using pubzub (MQTT)
 *
 * Copyright 2016 Marc Worrell
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

function Chatpage ()
{
    ubf.add_spec("chatpage_presence", ["user_id", "name", "page_id", "status"]);
    ubf.add_spec("chatpage_msg", ["page_id", "msg_id", "created", "html"]);

    var initialModel = {
        status: "online",
        page_id: 1234,
        pages: [
            /*
            {
                rsc_id: 1234,
                html: "...",
                presence: [
                    {
                        user_id: 1,
                        html: "..."
                    }
                ],
                messages: [
                    {
                        msg_id: 2
                        html: "..."
                    }
                ],
                max_id: 2,
                view_id: 1
            }
            */
        ]
    };

    var view = function(model) {
        return {
            pages: "... pages ...",
            presence: "... presence ...",
            messages: "... messages ..."
        };
    };

    var receive = function(model, proposal) {
        return { counter: model.counter + proposal.add };
    };

    var ready = function(propose) {
        var $root = $(document.getElementById("app"));

        $root.on("click", "button#inc", function(_evt) {
            propose({ add: 1 });
        });
        $root.on("click", "button#decr", function(_evt) {
            propose({ add: -1 });
        });
    };

    var Main = meiosis.createComponent({
        view: view,
        ready: ready,
        receive: receive
    });

    var renderer = meiosisVanillaJs.renderer();

    meiosis.run({
        renderer: renderer.intoId(document, "app"),
        initialModel: initialModel,
        rootComponent: Main
    });
}

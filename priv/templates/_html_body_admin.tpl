{% if m.acl.user and m.acl.use.mod_admin %}

{% with m.rsc.page_admin_chatpage.id as admin_id %}
<div id="admin-chat">
    <button id="admin-chat-toggle" class="btn btn-success">
        <span class="fa fa-comment"></span>
    </button>

    {% with id|default:admin_id as chat_id %}
    <div id="chatpage-top">
        <div id="chatpage-rooms" class="padding" data-page-id="{{ chat_id }}" data-chat-name="mod_admin">
            <select class="form-control">
                {% if id.is_visible and id /= admin_id %}
                    <option value="{{ id }}/mod_admin">{_ Page _}: {{ id.title }}</option>
                {% endif %}
                {% if admin_id %}
                    <option value="{{ admin_id }}/mod_admin">{{ admin_id.title }}</option>
                {% endif %}
            </select>
        </div>

        <div id="chatpage-presence" class="chatpage-presence" data-page-id="{{ chat_id }}" data-chat-name="mod_admin">
        </div>

        <div style="display:none" id="chatpage-load-more">
            <div class="chatpage-load-more">
                <a href="#">{_ Load more _}</a>
            </div>
        </div>
    </div>

    <div class="chatpage-chat" id="chatpage-chat" data-page-id="{{ chat_id }}" data-chat-name="mod_admin" style="max-height: 100%">
    </div>

    <div id="chatpage-bottom">
        <div id="chatpage-form" class="chatpage-form" data-page-id="{{ chat_id }}" data-chat-name="mod_admin">
          {% include "_chatpage_form.tpl" %}
        </div>
    </div>
    {% endwith %}
</div>

{% lib
    "chatpage/z.autoexpand.js"
    "chatpage/sam-rooms.js"
    "chatpage/sam-form.js"
    "chatpage/sam-presence.js"
    "chatpage/sam-chat.js"
%}

{% javascript %}
$('#admin-chat-toggle').click(function() {
    $("body").toggleClass('chat');
    try { localStorage.setItem('admin-chat-visible', $("body").hasClass('chat')); } catch (E) { };
});

if (localStorage && localStorage.getItem('admin-chat-visible') === "true") {
    $("body").addClass('chat');
}

function chat_new_message(chat_id, msg) {
    var show_chat = false;

    if (!$("body").hasClass('chat')) {
        show_chat = true;
    } else if ($("#chatpage-rooms select").val() != chat_id) {
        if ($('#chatpage-form textarea').is(':focus')) {
            z_growl_add(msg.messages[0].html);
        } else {
            show_chat = true;
        }
    }
    if (show_chat) {
        setTimeout(function() {
            $("body").addClass('chat');
            cotonic.broker.publish(
                "chatpage/selectroom",
                { page_id: chat_id });
        },10);
    }
}

{% if admin_id %}
    cotonic.broker.subscribe(
        "bridge/origin/chatpage/{{ admin_id }}/mod_admin",
        function(msg) {
            chat_new_message("{{ admin_id }}/mod_admin", msg.payload);
        });
{% endif %}
{% if id and id /= admin_id %}
    cotonic.broker.subscribe(
        "bridge/origin/chatpage/{{ id }}/mod_admin",
        function(msg) {
            chat_new_message("{{ id }}/mod_admin", msg.payload);
        });
{% endif %}

{% endjavascript %}

{% endwith %}

{% endif %}

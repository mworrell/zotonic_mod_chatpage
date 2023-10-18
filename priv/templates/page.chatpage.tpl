{% extends "page.tpl" %}

{% block title %}{{ id.title }}{% endblock %}

{% block html_head_extra %}
    {% lib "chatpage/chatpage.css" %}
{% endblock %}

{% block content %}

{# http://bootdey.com/snippets/view/chat-room #}

{% if m.acl.use.mod_chatpage %}
    <div class="chatpage-wrap">
        <div id="chatpage-rooms" data-page-id="{{ id }}">
            <select class="form-control">
                {% for _title,id in m.search[{all_bytitle cat=`chatpage`}] %}
                    {% if id.is_visible %}
                        <option value="{{ id }}/default">{{ id.title }}</option>
                    {% endif %}
                {% endfor %}
            </select>
        </div>

        <div id="chatpage-presence" class="chatpage-presence" data-page-id="{{ id }}">
        </div>

        <div id="chatpage-chat" class="chatpage-chat" data-page-id="{{ id }}">
        </div>

        <div id="chatpage-form" class="chatpage-form" data-page-id="{{ id }}">
            {% include "_chatpage_form.tpl" %}
        </div>

        <div style="display:none" id="chatpage-load-more">
            <div class="chatpage-load-more">
                <a href="#">{_ Load more _}</a>
            </div>
        </div>
    </div>
{% else %}
    <p class="alert alert-warning">{_ Sorry, you can’t use the chat. _}</p>
{% endif %}

{% endblock %}

{% block _js_include_extra %}
    {% lib
        "chatpage/z.autoexpand.js"
        "chatpage/sam-rooms.js"
        "chatpage/sam-form.js"
        "chatpage/sam-presence.js"
        "chatpage/sam-chat.js"
    %}
{% endblock %}

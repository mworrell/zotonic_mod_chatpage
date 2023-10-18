{% with m.rsc[q.user_id].id as user_id %}
<span class="chat-presence" data-user-id="{{ user_id }}" data-client-id="{{ q.client_id|escape }}">
    {% if user_id %}
        <span>{% include "_name.tpl" id=user_id %}</span>
    {% else %}
        <span class="glyphicon glyphicon-user"></span>
    {% endif %}
</span>
{% endwith %}

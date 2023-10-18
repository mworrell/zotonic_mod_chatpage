<div class="left clearfix" data-user-id="{{ post.user_id }}">
    <span class="chat-img">
        {% if post.user_id.depiction as depict %}
            {% image depict mediaclass="person-thumbnail" %}
        {% else %}
            <img src="/lib/images/noun/user.png" />
        {% endif %}
    </span>
    <div class="chat-body clearfix">
        <div class="chat-header">
            <strong>{% if post.user_id %}{% include "_name.tpl" id=post.user_id %}{% else %}<span class="glyphicon glyphicon-user"></span>{% endif %}</strong>
            <small class="pull-right text-muted"><i class="fa fa-clock-o"></i> {{ post.created|date:"Y-m-d H:i:s"}}</small>
        </div>

        <div class="chat-msg">
            {{ post.message }}
        </div>
    </div>
</div>

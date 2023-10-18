
/* Automatically expand textarea when typing */
$.widget("ui.autoexpand", {
    _init: function() {
        var self = this;

        $(this.element).on('keyup', function(e) {
            let wantedHeight = this.scrollHeight +
                                parseFloat($(this).css("borderTopWidth")) +
                                parseFloat($(this).css("borderBottomWidth"));
            wantedHeight = Math.min(wantedHeight, self.options.maxheight);
            while ($(this).outerHeight() < wantedHeight) {
                $(this).height($(this).height()+1);
            }
        });
    }
});

$.ui.autoexpand.defaults = {
    maxheight: 200
};

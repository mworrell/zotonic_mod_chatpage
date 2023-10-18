
/* Automatically expand textarea when typing */
$.widget("ui.autoexpand", {
    _init: function() {
        const self = this;
        const $input = $(this.element);

        self.options.minheight = $input.parent().height();

        function setHeight() {
            let wantedHeight = $input.get(0).scrollHeight +
                                parseFloat($input.css("borderTopWidth")) +
                                parseFloat($input.css("borderBottomWidth"));
            wantedHeight = Math.min(wantedHeight, self.options.maxheight);
            wantedHeight = Math.max(wantedHeight, self.options.minheight);
            while ($input.outerHeight() < wantedHeight) {
                $input.height($input.height()+1);
            }
        }

        setHeight();

        $input.on('keyup', function(e) {
            setHeight();
        });
    }
});

$.ui.autoexpand.defaults = {
    maxheight: 200,
    minheight: 10
};

"use strict";

// module WebIDL

exports.parseImpl = function(s) {
    return function() {
        return require('../../bower_components/webidl2').parse(s);
    };
};

"use strict";

// module WebIDL

exports.parseImpl = function(s) {
    return function() {
        return require('webidl2').parse(s);
    };
};

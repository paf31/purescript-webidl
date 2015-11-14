"use strict";

// module WebIDL

exports.parse = function(s) {

    return require('webidl2').parse(s);
};

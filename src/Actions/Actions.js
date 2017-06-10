/* global exports */
"use strict";

exports.tellImpl = function(client, message) {
    return client.tell(message);
};

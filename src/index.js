'use strict';

var Covfefe = require('Covfefe');

exports.realDonaldTrump = function(request, response) {
    var app = new App({request, response});

    var actionMap = new Map();
    actionMap.set('tweet', Covfefe.tellTweets);
    app.handleRequest(actionMap);
};

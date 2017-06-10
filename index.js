'use strict';

process.env.DEBUG = 'actions-on-google:*';
const App = require('actions-on-google').ApiAiApp;
const Covfefe = require('./src/Covfefe/index.js');

exports.tweet = function(request, response) {
    var app = new App({request, response});

    var actionMap = new Map();

    actionMap.set('tweet', Covfefe.tellTweets(app));
    app.handleRequest(actionMap);
};

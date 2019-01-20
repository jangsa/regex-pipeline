'use strict';

const Elm = require('./elm/Main.elm');
const mountNode = document.getElementById('root');

var app = Elm.Main.init({
  node: mountNode
});


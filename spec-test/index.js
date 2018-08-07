#!/usr/bin/env node

var Elm = require("./elm.js");
var app = Elm.Main.worker();

app.ports.toJS.subscribe(function(data) {
    if (data == null) {
        process.exit(1);
    } else {
        console.log(JSON.stringify(data, null, 0));
        process.exit(0);
    }
});

var ret = "";

process.stdin.setEncoding("utf8");

process.stdin.on("readable", function() {
    var chunk;
    while ((chunk = process.stdin.read())) {
        ret += chunk;
    }
});

process.stdin.on("end", function() {
    app.ports.fromJS.send(ret);
});

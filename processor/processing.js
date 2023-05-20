// blogex
// processing.js

const katex = require("katex");
const hljs = require("highlight.js");
const postcss = require("postcss");
const autoprefixer = require("autoprefixer");
const tailwindcss = require("tailwindcss");
const net = require("net");

// https://postcss.org/api/
function apply_postcss(out_folder, input, postConfigString) {
    var output = "";
    try {
        var tailConfig = JSON.parse(postConfigString);
        tailConfig["content"] = [out_folder + "/**/*.html"];
        output = "r:" + postcss([autoprefixer, tailwindcss(tailConfig)])
            .process(input, { from: undefined }).css;
    } catch (e) {
        output = "e:" + e.toString();
    }
    return output;
}

// https://katex.org/docs/api.html
function katex_render(input, display_true) {
    var output = "";
    try {
        output = "r:" + katex.renderToString(input, {
            displayMode: display_true
        });
    } catch (e) {
        output = "e:" + e.toString();
    }
    return output;
}

// https://highlightjs.org/
function highlight_code(input, code_lang) {
    var output = "";
    try {
        output = "r:" + hljs.highlight(input, {language: code_lang}).value;
    } catch (e) {
        output = "e:" + e.toString();
    }
    return output;
}

// Main

var input = [];
var output = ""
var buf = new Buffer.from("");
var buf_len = 0;
var in_size = -1;

port_num = parseInt(process.argv[2]);

var client = new net.Socket();

client.connect(port_num, "localhost", function() {});

function get_num(buf) {
    var out_str = buf.toString("utf-8");
    var n = out_str.indexOf("@");
    if (n == -1) {
        return -1;
    } else {
        var m = parseInt(out_str.substring(0, n));
        return Buffer.byteLength(out_str.substring(0, n + 1)) + m;
    }
}

function strip_head(buf) {
    var out_str = buf.toString("utf-8");
    return out_str.substring(out_str.indexOf("@") + 1);
}

client.on("data", function(data) {
    // Read data
    buf = Buffer.concat([buf, data]);
    buf_len += data.length;
    if (in_size == -1) {
        in_size = get_num(buf);
    }
    if (buf_len != in_size) {
        return;
    }

    // New data
    input.push(strip_head(buf));
    buf = Buffer.from("");
    buf_len = 0;
    in_size = -1;

    // Return things
    if (input[0] == "eqinline" && input.length == 2) {
        output = katex_render(input[1], false);
        input = []
    } else if (input[0] == "eqdisplay" && input.length == 2) {
        output = katex_render(input[1], true);
        input = [];
    } else if (input[0] == "highlight" && input.length == 3) {
        output = highlight_code(input[2], input[1]);
        input = [];
    } else if (input[0] == "postcss" && input.length == 4) {
        output = apply_postcss(input[1], input[2], input[3]);
        input = [];
    } else {
        output = "r:ok";
    }

    // By default, encoding of string is in utf-8
    client.write(Buffer.byteLength(output, "utf-8").toString() + "@" + output);
});

client.on("close", function() {});

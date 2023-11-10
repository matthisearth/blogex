// blogex
// processing.js

import autoprefixer from "npm:autoprefixer@^10.4.12";
import hljs from "npm:highlight.js@^11.6.0";
import katex from "npm:katex@^0.16.2";
import postcss from "npm:postcss@^8.4.18";
import tailwindcss from "npm:tailwindcss@^3.1.8";

// https://postcss.org/api/
async function apply_postcss(out_folder, input, postConfigString) {
    var output = "";
    try {
        var tailConfig = JSON.parse(postConfigString);
        tailConfig["content"] = [out_folder + "/**/*.html"];
        var result = await postcss([autoprefixer, tailwindcss(tailConfig)])
            .process(input, { from: undefined });
        output = "r:" + result.css;
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

var input = Deno.args;

// Return things
if (input[0] == "eqinline" && input.length == 2) {
    console.log(katex_render(input[1], false));
} else if (input[0] == "eqdisplay" && input.length == 2) {
    console.log(katex_render(input[1], true));
} else if (input[0] == "highlight" && input.length == 3) {
    console.log(highlight_code(input[2], input[1]));
} else if (input[0] == "postcss" && input.length == 4) {
    apply_postcss(input[1], input[2], input[3]).then(output => {
        console.log(output);
    });
} else {
    console.log("e:input");
}


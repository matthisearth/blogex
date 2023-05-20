# blogex

Blogex is a very simple static site generator written in Haskell which takes
LaTeX files as inputs and generates HTML files. It supports server-side
rendering of equations using [KaTeX](https://katex.org/), syntax highlighting
using [highlight.js](https://highlightjs.org/), as well as CSS processing with
[PostCSS](https://postcss.org/) and [tailwindcss](https://tailwindcss.com/).
One of the current main speed bottlenecks is the NodeJS processing of equations,
code displays and CSS files.

## Setup

```bash
stack install
cd processor && npm install
```

## Usage

To see how the templating language works, see the folder `test/in/` which
contains a simple layout and an example blog website; the resulting output is
stored in `test/out/`. To run blogex on the input folder `in-folder/` and
store the result in `out-folder/` run the command below.

```bash
blogex path/to/processor/processing.js in-folder/ out-folder/
```

## License

This project is licensed under the MIT License.

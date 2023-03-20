# Installation Instructions

The program is written in [elm](https://elm-lang.org/) programming language.
Elm can be installed on your machine by following
[this](https://guide.elm-lang.org/install/elm.html) guideline.

## Run the following commands from a shell.
Go to the Website directory and compile elm program
to main.js by running the compile script.

```bash
cd Website/
./compile
```

The `compile` script is actually a bash script containing the
following code:

```bash
#!/bin/bash
elm make src/Main.elm --output=main.js
```

As can be seen the elm code is compiled to javascript.
`main.js` is invoked in the `index.html` page.
Open `index.html` in a browser to see the app.

# Installation

## Steps
First, ensure you have installed ocaml in you PC. If not, follow the following:
https://cs3110.github.io/textbook/chapters/preface/install.html

Once you are done, make sure you open the project in the VSCode.
To open the project in the VSCode, follow the following:
Start VSCode, click on Open From.., and choose the download folder to open. 

Make sure you run the below install code in VSCode Terminal!
To start a terminal in VSCode, follow the following:
Move mouse to top of the VSCode, click on Terminal -> New Terminal

Second, update opam:
```sh
opam update
```

Third, upgrade opam:
```sh
opam upgrade
```

Fourth, install library ANSITerminal
```sh
opam install ANSITerminal
```

Fifth, build dune
```sh
make build
```

Last, play the game
```sh
make run
```
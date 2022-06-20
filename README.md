# Practical Verification of Concurrent Haskell Programs

## Description

This project was submitted as part of the [Research Project](https://cse3000-research-project.github.io/2022/Q4) of the [TU Delft](https://github.com/CSEdelft).

*The aim of this project is the formal verification of concurrent Haskell programs through Agda2hs.*

Agda2hs is a tool for automatic translation of Agda code to Haskell. Its aim is to produce readable Haskell code that compiles correctly. In this project, I investigated whether Agda2hs can be used to produce correct Haskell code from Concurrent Haskell models ported to and verified in Agda.

Background, method, the most important implementation details, results and limitations are all presented in the paper submitted for this project, which can be found in `src/Doc/`. Here you may also find suggestions for extensions on the present work.

## Structure

- `src/Data/`: contains all Agda code discussed in the paper as well as some additional unused modules. Comments in combination with the paper should be enough to understand what is going on.
- `src/Archive/`: contains some modules that were discarded, usually because of some issue with the Agda2hs translation was discovered at some point. Documentation on these files can be found in `src/Doc/`.
- `lib/Data/`: contains Agda2hs's translations to Haskell of all modules contained in `Everything.agda` (that is, all files relevant to the latest stages of the project).
- `demo/main.hs`: contains a executable Haskell demo. Is made to be used with the programs from `lib/Programs.hs` (which are translated from `src/Data/Programs.agda`).

## Prerequisites

To use this project it is necessary to have cabal, ghc and make installed. If
you don't have these installed you can follow the instructions below, else you
can continue on to [here](#dependencies).

### For Windows (Using Chocolately):

It is recommended that you install chocolately as a package manager that allows
for installing software by using the built-in terminal in Windows. For the
latest instruction on installing chocolately [click
here](https://chocolatey.org/install#individual) and use the individual
installation method.

To install all prerequisites execute the following commands in your terminal:
```
choco install ghc cabal make
refreshenv
```

Now you can go ahead and install the dependencies.

## Dependencies

This project relies on a custom fork of both [agda2hs] and [Agda].
To build them from source, do the following:

```
git clone https://github.com/mschifferstein/concurrent-haskell-verification
cd concurrent-haskell-verification
cabal install Agda    # If there are conflicting dependencies for base use the following flag: --allow-newer=base
cabal install agda2hs # If there are conflicting dependencies for base use the following flag: --allow-newer=base
```

Building Agda may take a while.

In order to use the Haskell prelude of `agda2hs` from your Agda code, you also
need to tell Agda where to locate the library.

Clone Jesper Cockx's fork of agda2hs:

```
git clone -b erasure-annotations https://github.com/jespercockx/agda2hs
```
### For Unix:

Inside the file `~/.agda/libraries`, add the following line:

```
/your/path/to/agda2hs/agda2hs.agda-lib
```

### For Windows:

First off execute the following command:

```
(test-path -path $home\AppData\Roaming\agda\libraries -pathtype Leaf) ? (echo "File not created, it already exists") : (new-item -path $home\AppData\Roaming\agda\libraries)
```

Either add the following line to `C:\Users\<USER>\AppData\Roaming\agda\libraries` or alternatively when using powershell `$home\AppData\Roaming\agda\libraries`, 

```
</your/path/to/agda2hs/agda2hs.agda-lib>
```

Or run the following command in powershell:

```
add-content $home\AppData\Roaming\agda\libraries "`n</your/path/to/agda2hs/agda2hs.agda-lib>"
```

*You will have to create this file if it does not exist.*

## Usage

You should be good to go. Open any file in the `src/` directory inside your IDE of choice and you should be able to use the Haskell prelude in your code without any issue.

Running `make` at the root of the project will:
- compile `Everything.agda` using `agda2hs`. All agda files imported in `Everything.agda` are compiled to Haskell automatically.
- compile the Haskell library generated from the Agda files in `lib/`.
- comile the demo Haskell executable in `demo/`

[Agda]:    https://github.com/agda/Agda
[agda2hs]: https://github.com/agda/agda2hs

To run the demo executable, just launch `cabal run demo`.

To test out the library in a REPL, use `cabal repl project`.

## Acknowledgements
I thank Lucas Escot for providing a [template](https://github.com/flupe/verification-template) along with the instructions on prerequisites and dependencies. 

I furthermore would like to thank him and Jesper Cockx as my supervisors, as well as my peers, for their feedback and readiness to help with any implementation issues I have encountered along the way.
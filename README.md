# `miniBill/elm-fast-dict` [![Build Status](https://github.com/miniBill/elm-fast-dict/workflows/CI/badge.svg)](https://github.com/miniBill/elm-fast-dict/actions?query=branch%3Amain)

This is a drop-in replacement for `Dict` from `elm/core`. The suggested way to use it is by replacing `import Dict` with `import FastDict as Dict` in your project.

The main differences are:

1. This is not a built-in type, so you can't use `==` with it. If you do you may get false negatives (dictionaries which are equal but that `==` considers different) - use `FastDict.equals` to check for equality;

The suggested way to 

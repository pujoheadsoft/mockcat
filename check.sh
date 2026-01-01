#!/bin/sh
stack test \
  --force-dirty \
  --ghc-options "-Wall -Werror=unused-do-bind"
#!/bin/env bash

cabal update --project-dir=workspace
cabal build --project-dir=workspace all

echo Lamba binary location:
cabal list-bin --project-dir=workspace gpg-decrypt-app

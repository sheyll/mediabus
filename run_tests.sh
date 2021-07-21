#!/usr/bin/env bash

set -e

nix build -L -f default.nix mediabus.components.tests

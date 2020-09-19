#!/bin/sh
nix-shell -p haskellPackages.wai-app-static --command "warp -d result -p 8082"

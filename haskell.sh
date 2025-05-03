#!/usr/bin/bash
export HSPRO="/c/users/julia/haskellexercises"
export HSGHC="/c/ghcup/bin"

function ghcup(){
    ghcup=$HSGHC"/ghcup.exe"
    !ghcup
}
export function ghcup

function ghci(){
    execute=$HSGHC"/ghci.exe"
    !$execute
}
export function ghci

function ghc(){
    execute=$HSGHC"/ghc.exe" 
    !$execute $1 $2
}
export function ghc

function cabal(){
    execute=$HSGHC"/cabal.exe"
    !$execute $1 $2 $3
}
export function cabal


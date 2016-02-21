module Main where

import Signal exposing (Signal)

import ElmTest exposing (consoleRunner)
import Console exposing (IO, run)
import Task

import Src.NotQuiteLisp.Tests as Tests

console : IO ()
console = consoleRunner Tests.all

port runner : Signal (Task.Task x ())
port runner = run console

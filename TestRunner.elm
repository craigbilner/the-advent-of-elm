module Main where

import Signal exposing (Signal)

import ElmTest exposing (..)
import Console exposing (IO, run)
import Task

import Src.NotQuiteLisp.Tests as NQLTests
import Src.IWasToldThereWouldBeNoMath.Tests as IWTTWBNMTests

tests : Test
tests = ElmTest.suite ""  [NQLTests.all,  IWTTWBNMTests.all]

console : IO ()
console = ElmTest.consoleRunner tests

port runner : Signal (Task.Task x ())
port runner = run console

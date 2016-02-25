module Main where

import Signal exposing (Signal)

import ElmTest exposing (..)
import Console exposing (IO, run)
import Task

import Src.NotQuiteLisp.Tests as NQLTests
import Src.IWasToldThereWouldBeNoMath.Tests as IWTTWBNMTests
import Src.PerfectlySpherical.Tests as PSTests
import Src.PerfectlySpherical2.Tests as PS2Tests
import Src.DoesntHeHaveInterns.Tests as DHHI

tests : Test
tests = ElmTest.suite ""  [ NQLTests.all
                          , IWTTWBNMTests.all
                          , PSTests.all
                          , PS2Tests.all
                          , DHHI.all
                          ]

console : IO ()
console = ElmTest.consoleRunner tests

port runner : Signal (Task.Task x ())
port runner = run console

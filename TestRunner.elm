module Main where

import Signal exposing (Signal)

import ElmTest exposing (..)
import Console exposing (IO, run)
import Task

import NotQuiteLisp.Tests as NQLTests
import IWasToldThereWouldBeNoMath.Tests as IWTTWBNMTests
import PerfectlySpherical.Tests as PSTests
import PerfectlySpherical2.Tests as PS2Tests
import DoesntHeHaveInterns.Tests as DHHI
import ProbablyAFire.Tests as PAF
import SomeAssemblyRequired.Tests as SAR
import Matchsticks.Tests as M
import AllInASingleNight.Tests as AIASN
import ElvesLookElvesSay.Tests as ELES

tests : Test
tests = ElmTest.suite ""  [ NQLTests.all
                          , IWTTWBNMTests.all
                          , PSTests.all
                          , PS2Tests.all
                          , DHHI.all
                          , PAF.all
                          , SAR.all
                          , M.all
                          , AIASN.all
                          , ELES.all
                          ]

console : IO ()
console = ElmTest.consoleRunner tests

port runner : Signal (Task.Task x ())
port runner = run console

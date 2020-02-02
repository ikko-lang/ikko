module Main where

import Test.HUnit

import Types

main = runTestTT tests

tests :: Test
tests =
  TestList
  [ TestLabel "unApplyTypes" testUnapplyTypes ]

labeled name assertion = TestLabel name $ TestCase assertion

testUnapplyTypes :: Test
testUnapplyTypes =
  TestList
  [ labeled "no applications" $ do
      let tx = tvar "x"
      let result = unApplyTypes tx
      assertEqual "" (tx, []) result

  , labeled "one application" $ do
      let tx = tvar "x"
      let ty = tvar "y"
      let result = unApplyTypes (TAp tx ty)
      assertEqual "" (tx, [ty]) result

  , labeled "two applications" $ do
      let tx = tvar "x"
      let ty = tvar "y"
      let tz = tvar "z"
      let result = unApplyTypes (TAp (TAp tx ty) tz)
      assertEqual "" (tx, [ty, tz]) result

  , labeled "reverses applyTypes" $ do
      let tx = tvar "x"
      let ty = tvar "y"
      let tz = tvar "z"
      let result = unApplyTypes $ applyTypes tx [ty, tz]
      assertEqual "" (tx, [ty, tz]) result
  ]


tcon :: String -> [Type] -> Type
tcon name types =
  applyTypes (TCon name $ kindN $ length types) types


tgenN :: Int -> Type
tgenN n = TGen n Star

tvar :: String -> Type
tvar name = TVar $ TyVar name Star

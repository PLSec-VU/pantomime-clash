{-# LANGUAGE DuplicateRecordFields #-}

module Pantomime.Clash.NonInterference
  ( Circuit

  , SimulationNI (..)
  , simulationNI

  , SimulatorExistNI (..)
  , tickStateCorrespondence
  , projectionCoherence
  ) where

import Data.Bifunctor (Bifunctor (..))
import Data.Composition ((.:))
import Pantomime.BuiltIn qualified as Pantomime

type Circuit s i o = s -> i -> (s, o)

-- TODO: I think we should name this "constructive", or "simulator-based".
-- That would be slightly more descriptive in terms of what the check does.
data SimulationNI si sl ss i i' o o' where
  SimulationNI ::
    { observation :: o -> o'
    , implementation :: Circuit si i o
    , leakage :: Circuit sl i i'
    , simulator :: Circuit ss i' o'
    , projection :: si -> (sl, ss)
    } -> SimulationNI si sl ss i i' o o'

simulationNI
  :: Eq o'
  => Eq ss
  => Eq sl
  => SimulationNI si sl ss i i' o o'
  -> si
  -> i
  -> Pantomime.Bool
simulationNI SimulationNI { .. } = do
  let c1 = bimap projection observation .: implementation
  let c2 si i = do
        let (sl, ss) = projection si
        let (sl', x) = leakage sl i
        let (ss', o) = simulator ss x
        ((sl', ss'), o)

  \si i -> convert $ c1 si i == c2 si i

data SimulatorExistNI si sl ss i l o where
  SimulatorExistNI ::
    { implementation :: Circuit si i o
    , leakage :: Circuit sl i l
    , projection :: si -> (sl, ss)
    } -> SimulatorExistNI si sl ss i l o

-- TODO: Ideally, users would be able to write a single theory. Sadly, just
-- adding an && between these will likely be a bit slower than necessary. Also,
-- it will be a harder to distinguish where it failed. For now, I just separated
-- the two checks so one can query the solver twice.
tickStateCorrespondence
  :: Eq sl
  => SimulatorExistNI si sl ss i l o
  -> si
  -> i
  -> Pantomime.Bool
tickStateCorrespondence SimulatorExistNI { .. } = do
  let leakage' s i = do
        let (sl, _ss) = projection s
        let (sl', _o) = leakage sl i
        sl'
  let implementation' s i = do
        let (s', _o) = implementation s i
        let (sl', _ss') = projection s'
        sl'

  \s i -> convert $ leakage' s i == implementation' s i

projectionCoherence
  :: Eq o
  => Eq l
  => Eq ss
  => SimulatorExistNI si sl ss i l o
  -> si
  -> i
  -> si
  -> i
  -> Pantomime.Bool
projectionCoherence SimulatorExistNI { .. } = do
  let leakage' s i = do
        let (sl, ss) = projection s
        let (_sl', o) = leakage sl i
        (ss, o)
  let implementation' s i = do
        let (s', o) = implementation s i
        let (_sl', ss') = projection s'
        (ss', o)

  \s i s' i' -> convert do
    let pre = leakage' s i == leakage' s' i'
    let post = implementation' s i == implementation' s' i'
    not pre || post

convert :: Bool -> Pantomime.Bool
convert value = case value of
  True -> Pantomime.True
  False -> Pantomime.False

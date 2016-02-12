{-# LANGUAGE OverloadedStrings #-}

module InitData (vehicleTbl) where

import Types

import qualified Data.IntMap.Lazy as IM (IntMap, fromList)


vehicleTbl :: IM.IntMap Vehicle
vehicleTbl = IM.fromList $ let tuples = [ ("vin 0", 2016, "M.Plus",  [])
                                        , ("vin 1", 2015, "Forever", [ Issue Battery    Low
                                                                     , Issue Electrical High
                                                                     , Issue Brakes     Med ])
                                        , ("vin 2", 2014, "Pure",    []) ]
                           in zipWith f [0..] tuples
  where
    f i (v, y, m, is) = (i, Vehicle (Just i) v y m is)

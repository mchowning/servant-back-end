{-# LANGUAGE OverloadedStrings #-}

module InitData (vehicleTbl) where

import Types

import qualified Data.IntMap.Lazy as IM


vehicleTbl :: IM.IntMap Vehicle
vehicleTbl = IM.fromList vehicleAssocs


vehicleAssocs :: [(Int, Vehicle)]
vehicleAssocs = map mkVehicleAssoc [ (0, "vin 0", 2016, "M.Plus",  "black", [])
                                   , (1, "vin 1", 2015, "Forever", "white", [ Issue Battery    Low
                                                                            , Issue Electrical High
                                                                            , Issue Brakes     Med ])
                                   , (2, "vin 2", 2014, "Pure",    "blue",  []) ]
  where
    mkVehicleAssoc (i, v, y, m, c, is) = (i, Vehicle (Just i) v y m c is)

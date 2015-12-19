{-# LANGUAGE OverloadedStrings #-}

module InitData where

import Types


artists :: [Artist]
artists = [ Artist "They Might Be Giants"
              [ Album "They Might Be Giants" 1986 (Len 38 29)
                  [ Song "Everything Right Is Wrong Again"      (Len 2 20)
                  , Song "Put Your Hand Inside The Puppet Head" (Len 2 12)
                  , Song "Number Three"                         (Len 1 27)
                  , Song "Don't Let's Start"                    (Len 2 36)
                  , Song "Hide Away Folk Family"                (Len 3 21)
                  , Song "32 Footsteps"                         (Len 1 36)
                  , Song "Toddler Hiway"                        (Len 0 25)
                  , Song "Rabid Child"                          (Len 1 31)
                  , Song "Nothing's Gonna Change My Clothes"    (Len 1 58)
                  , Song "(She Was A) Hotel Detective"          (Len 2 10)
                  , Song "She's An Angel"                       (Len 2 37)
                  , Song "Youth Culture Killed My Dog"          (Len 2 51)
                  , Song "Boat Of Car"                          (Len 1 15)
                  , Song "Absolutely Bill's Mood"               (Len 2 38)
                  , Song "Chess Piece Face"                     (Len 1 21)
                  , Song "I Hope That I Get Old Before I Die"   (Len 1 58)
                  , Song "Alienation's For The Rich"            (Len 2 25)
                  , Song "The Day"                              (Len 1 27)
                  , Song "Rhythm Section Want Ad"               (Len 2 21) ]
              , Album "Lincoln" 1988 (Len 39 32)
                  [ Song "Ana Ng"                               (Len 3 23)
                  , Song "Cowtown"                              (Len 2 21)
                  , Song "Lie Still, Little Bottle"             (Len 2 06)
                  , Song "Purple Toupee"                        (Len 2 40)
                  , Song "Cage & Aquarium"                      (Len 1 10)
                  , Song "Where Your Eyes Don't Go"             (Len 3 06)
                  , Song "Piece Of Dirt"                        (Len 2 00)
                  , Song "Mr. Me"                               (Len 1 52)
                  , Song "Pencil Rain"                          (Len 2 42)
                  , Song "The World's Address"                  (Len 2 24)
                  , Song "I've Got A Match"                     (Len 2 36)
                  , Song "Santa's Beard"                        (Len 1 55)
                  , Song "You'll Miss Me"                       (Len 1 53)
                  , Song "They'll Need A Crane"                 (Len 2 33)
                  , Song "Shoehorn With Teeth"                  (Len 1 13)
                  , Song "Stand On Your Own Head"               (Len 1 16)
                  , Song "Snowball In Hell"                     (Len 2 31)
                  , Song "Kiss Me, Son Of God"                  (Len 1 54) ] ] ]

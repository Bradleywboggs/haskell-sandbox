--Bogglets.hs
module Bogglets where

  
  data Bogglet        = Bradley | Krystle | Josiah | Lileigh | James | Gilead | BabyBogglet | ImaginaryBogglet deriving Show
  data Fave           = Flowers | Cookies | Lollipops | NerfGuns | Pranks | Cars | BabyDolls | Bagels | Mommy deriving Show
  data LeastFavorite  = StinkyDiapers | Needles | PoppedBalloons  | RuinedCookies | RuinedLollipops | SmellingPoop | Chores | Cereal deriving Show
--  data BoggletQuantity = BQ1 | BQ2 | BQ3 | BQ4 | BQ5 | BQ6 | BQ33 | BQ 34 deriving Show

  findABogglet :: Num a => a -> Bogglet
  findABogglet 1 = Josiah
  findABogglet 2 = Lileigh
  findABogglet 3 = James
  findABogglet 4 = Gilead
  findABogglet 5 = BabyBogglet
  findABogglet 33 = Bradley
  findABogglet 32 = Krystle
  findABogglet _ = ImaginaryBogglet

  boggletFaves :: Bogglet -> (Fave, Fave) 
  boggletFaves Josiah  = (Nerf_Guns, Bagels)
  boggletFaves Lileigh = (BabyDolls, Lollipops)
  boggletFaves James   = (Cookies, Cars)
  boggletFaves Gilead  = (Mommy, BabyDolls)
  boggletFaves _       = (Pranks, Mommy)

  boggletLeastFaves :: Bogglet -> LeastFavorite
  boggletLeastFaves Josiah           = Cereal
  boggletLeastFaves Lileigh          = Chores
  boggletLeastFaves James            = Ruined_Cookies
  boggletLeastFaves Gilead           = Popped_Balloons
  boggletLeastFaves ImaginaryBogglet = Smelling_Poop
  boggletLeastFaves _                = Ruined_Lollipops


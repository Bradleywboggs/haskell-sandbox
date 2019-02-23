--Bogglets.hs
module Bogglets where

  
  data Bogglet        = Bradley | Krystle | Josiah | Lileigh | James | Gilead | BabyBogglet | ImaginaryBogglet deriving Show
  data Fave           = Flowers | Cookies | Lollipops | Nerf_Guns | Pranks | Cars | BabyDolls | Bagels | Mommy deriving Show
  data LeastFavorite  = StinkyDiapers | Needles | PoppedBalloons  | MissingCars | RuinedLollipops | SmellingPoop | Chores | Cereal deriving Show
  --   data BoggletQuantity = One | Two | Three | Four | Five | Six deriving Show

  findABogglet :: Integer -> Bogglet
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
  boggletLeastFaves James            = MissingCars
  boggletLeastFaves Gilead           = PoppedBalloons
  boggletLeastFaves ImaginaryBogglet = SmellingPoop
  boggletLeastFaves _                = RuinedLollipops


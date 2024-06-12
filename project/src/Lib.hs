module Lib where

import Text.Printf (printf)

-- Define the Fuel data type
data Fuel = Fuel
  { fuelName :: String,
    thrust :: Double
  } deriving (Show)

-- Define the LaunchSite data type
data LaunchSite = LaunchSite
  { siteName :: String,
    latitude :: Double,
    altitude :: Double,
    rotationalVelocity :: Double
  } deriving (Show)

-- Define some sample fuels and launch sites
liquidHydrogenOxygen :: Fuel
liquidHydrogenOxygen = Fuel "Ciekły wodór + Ciekły tlen" 5000.0

canaveral :: LaunchSite
canaveral = LaunchSite "Canaveral (NASA)" 28.5721 0.0 (2 * pi * cos (28.5721 * pi / 180) / (24 * 60 * 60))

guiana :: LaunchSite
guiana = LaunchSite "Gujana Francuska (ESA)" 5.236 0.0 (2 * pi * cos (5.236 * pi / 180) / (24 * 60 * 60))

kennedy :: LaunchSite
kennedy = LaunchSite "Kennedy (NASA)" 28.5721 0.0 (2 * pi * cos (28.5721 * pi / 180) / (24 * 60 * 60))

vandenberg :: LaunchSite
vandenberg = LaunchSite "Vandenberg (NASA)" 34.742 0.0 (2 * pi * cos (34.742 * pi / 180) / (24 * 60 * 60))

warszawa :: LaunchSite
warszawa = LaunchSite "Warszawa" 52.2297 113.0 (2 * pi * cos (52.2297 * pi / 180) / (24 * 60 * 60))

olsztyn :: LaunchSite
olsztyn = LaunchSite "Olsztyn" 53.7799 135.0 (2 * pi * cos (53.7799 * pi / 180) / (24 * 60 * 60))

xichang :: LaunchSite
xichang = LaunchSite "Xichang (CASC)" 28.246 1788.0 (2 * pi * cos (28.246 * pi / 180) / (24 * 60 * 60))

taiyuan :: LaunchSite
taiyuan = LaunchSite "Taiyuan (CASC)" 37.208 1500.0 (2 * pi * cos (37.208 * pi / 180) / (24 * 60 * 60))

sriharikota :: LaunchSite
sriharikota = LaunchSite "Sriharikota (ISRO)" 13.733 13.0 (2 * pi * cos (13.733 * pi / 180) / (24 * 60 * 60))

-- Function to print launch site information
printLaunchSiteInfo :: LaunchSite -> IO ()
printLaunchSiteInfo site = do
  putStrLn "--------------------------------------"
  printf "Lotnisko:           %s\n" (siteName site)
  printf "Szerokość geograficzna: %.4f\n" (latitude site)
  printf "Wysokość:           %.1f m\n" (altitude site)
  printf "Prędkość kątowa:    %.8f rad/s\n" (rotationalVelocity site)
  putStrLn "--------------------------------------"

-- Function to create a custom launch site with user input
createCustomLaunchSite :: IO LaunchSite
createCustomLaunchSite = do
  putStrLn "---------------------"
  putStrLn "Tworzenie nowego lotniska"
  putStrLn "---------------------"
  putStrLn "Wprowadź nazwę lotniska:"
  name <- getLine
  putStrLn "Wprowadź szerokość geograficzną lotniska (stopnie):"
  latStr <- getLine
  let latitude = read latStr :: Double
  putStrLn "Wprowadź wysokość lotniska nad poziomem morza (m):"
  altStr <- getLine
  let altitude = read altStr :: Double
  let rotationalVelocity = 2 * pi * cos (latitude * pi / 180) / (24 * 60 * 60)
  putStrLn "---------------------"
  return $ LaunchSite name latitude altitude rotationalVelocity

-- Function to calculate the thrust of a rocket
calculateThrust :: Fuel -> LaunchSite -> Double -> Double -> Double
calculateThrust fuel site rocketMass launchAngle =
  let baseThrust = thrust fuel * rocketMass
      latitudeFactor = cos (latitude site * pi / 180)
      gravity = 9.81 * (1 - 2 * altitude site / 6356.8)
      coriolisForce = -2 * rotationalVelocity site * rocketMass * sin (latitude site * pi / 180)
      modifiedThrust = baseThrust * latitudeFactor + coriolisForce
  in modifiedThrust * sin (launchAngle * pi / 180) * gravity / 9.81

-- Function to print fuel information
printFuelInfo :: Fuel -> IO ()
printFuelInfo fuel = do
  putStrLn "--------------------------------------"
  printf "Paliwo: %s\n" (fuelName fuel)
  printf "Ciąg:   %.2f N\n" (thrust fuel)
  putStrLn "--------------------------------------"

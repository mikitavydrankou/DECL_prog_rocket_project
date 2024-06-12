module Main where

import Lib (printFuelInfo, printLaunchSiteInfo, calculateThrust, createCustomLaunchSite, liquidHydrogenOxygen, canaveral, guiana, kennedy, vandenberg, warszawa, olsztyn, xichang, taiyuan, sriharikota)

main :: IO ()
main = do
  printFuelInfo liquidHydrogenOxygen

  putStrLn "Wprowadź masę rakiety (kg):"
  rocketMassStr <- getLine
  let rocketMass = read rocketMassStr :: Double
  putStrLn "Wybierz lotnisko:"
  putStrLn "1. Canaveral (NASA)"
  putStrLn "2. Gujana Francuska (ESA)"
  putStrLn "3. Kennedy (NASA)"
  putStrLn "4. Vandenberg (NASA)"
  putStrLn "5. Warszawa"
  putStrLn "6. Olsztyn"
  putStrLn "7. Xichang (CASC)"
  putStrLn "8. Taiyuan (CASC)"
  putStrLn "9. Sriharikota (ISRO)"
  putStrLn "10. Wprowadź własne dane"
  launchSiteChoice <- getLine
  launchSite <- case launchSiteChoice of
                  "1" -> return canaveral
                  "2" -> return guiana
                  "3" -> return kennedy
                  "4" -> return vandenberg
                  "5" -> return warszawa
                  "6" -> return olsztyn
                  "7" -> return xichang
                  "8" -> return taiyuan
                  "9" -> return sriharikota
                  "10" -> createCustomLaunchSite
                  _ -> error "Nieprawidłowy wybór"
                     
  printLaunchSiteInfo launchSite

  putStrLn "Wprowadź kąt startu (stopnie):"
  launchAngleStr <- getLine
  let launchAngle = read launchAngleStr :: Double
  
  let calculatedThrust = calculateThrust liquidHydrogenOxygen launchSite rocketMass launchAngle
  putStrLn $ "Ciąg: " ++ show calculatedThrust ++ " N"


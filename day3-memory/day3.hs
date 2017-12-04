distance :: Int -> Int
distance input = distance' input 2 2 3 1 1 2

distance' :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int
distance' input value modInc modulus middle increments startValue | calcMod modulus value == 0 && value == input = floor (fromIntegral (modInc)/ fromIntegral 2) + middle
                                                       | value == input = stepsToMiddle + middle
                                                       | incTrue == True && calcMod modulus value == 0 = distance' input (value+1) (modInc+1) (modulus+modInc) (middle+1) 1 modulus
                                                       | calcMod modulus value == 0 = distance' input (value+1) (if calcMod increments 2 == 0 then (modInc+1) else (modInc)) (modulus+modInc) middle (increments+1) modulus
                                                       | otherwise = distance' input (value+1) modInc modulus middle increments startValue
                                                                    where
                                                                    calcMod n1 n2 = (n1 `mod` n2)
                                                                    checkModInc = increments
                                                                    (stepsToMiddle, incTrue) = (abs ( (modulus-value) - (ceiling (fromIntegral (modulus-startValue) / fromIntegral 2))), (if ((increments `mod` 4 == 0)) then True else False))
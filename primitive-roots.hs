-- Author : Gilang Ardyamandala Al Assyifa
-- Topic  : Primitive Roots and Quadratic Residues (Number Theory)
-- Project for my course (MA2252 Pengantar Teori Bilangan)
-- Handbook : Elementary Number Theory 6th, Kenneth H. Rosen
-- Tested with ghc 7.10

-- Relatively prime
rfPrime a b = gcd a b == 1

-- Euler totient Funtion
-- The number of Z+ that relatively prime to x
-- Precondition : x > 0
eulerTotient x = if x == 1 then 1 else iterET x (x-1)

iterET x n
    | n == 0 = 0
    | rfPrime x n = 1 + iterET x (n-1)
    | otherwise = iterET x (n-1)


-- Ordo modulo function
-- Lowest power(Z+) of a that congruance with 1(mod n)
-- Precondition : a relatively prime to n
ordo a n = iterOrdo a n 1

iterOrdo a n c
    | a^c `mod` n == 1 = c
    | otherwise = iterOrdo a n (c+1)


-- Primitive root funtion
-- r is modulo primitive root of n when ordo r n == eulerTotient n
-- Precondition : n > 0, r and n are relatively prime
-- Without no primitive root cases
primitiveRoot n = if (even n) then n+1 else iterPR 2 n

iterPR r n
    | ordo r n == eulerTotient n = r
    | otherwise = iterPR (r+1) n
    
-- Main program
main = print $ primitiveRoot 7
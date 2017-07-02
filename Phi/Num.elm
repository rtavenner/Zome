module Phi.Num exposing (PhiNum(..),getPhis,getOnes,toFloat,add,sub,mul,recip,div,zero,one,two,three,half,phi,phi2,phi3,phi2th,phi3th,phith,halve,neg,equal,less)

import Ratio exposing (Rational,fromInt)
import Ratio.Infix exposing (..)

--                   Phis     Ones
type PhiNum = PhiNum Rational Rational

intPhiNum : Int -> Int -> PhiNum
intPhiNum a b = PhiNum (fromInt a) (fromInt b)

getPhis : PhiNum -> Rational
getPhis (PhiNum p _) = p

getOnes : PhiNum -> Rational
getOnes (PhiNum _ o) = o

toFloat : PhiNum -> Float
toFloat (PhiNum p o) = 
    Ratio.toFloat p * (sqrt 5 + 1) / 2
    + Ratio.toFloat o 



add : PhiNum -> PhiNum -> PhiNum
add (PhiNum p1 o1) (PhiNum p2 o2) = (PhiNum (p1 |+| p2) (o1 |+| o2))

sub : PhiNum -> PhiNum -> PhiNum
sub (PhiNum p1 o1) (PhiNum p2 o2) = (PhiNum (p1 |-| p2) (o1 |-| o2))

mul : PhiNum -> PhiNum -> PhiNum
mul (PhiNum p1 o1) (PhiNum p2 o2) =
    PhiNum
        (p1 |*| p2 |+| p1 |*| o2 |+| o1 |*| p2)
        (p1 |*| p2 |+| o1 |*| o2)

recip : PhiNum -> PhiNum
recip (PhiNum p o) =
    PhiNum
        (             (p)       |/| (p |*| p |-| p |*| o |-| o |*| o))
        (Ratio.negate (p |+| o) |/| (p |*| p |-| p |*| o |-| o |*| o))

div : PhiNum -> PhiNum -> PhiNum
div a b = mul a (recip b)


zero = intPhiNum 0 0
one = intPhiNum 0 1
two = intPhiNum 0 2
three = intPhiNum 0 3
half = div one two
phi = intPhiNum 1 0
phi2 = mul phi phi
phi3 = mul phi phi2
phith = sub phi one
phi2th = mul phith phith
phi3th = mul phith phi2th

halve = mul half
neg = sub zero


equal (PhiNum a b) (PhiNum c d) = a == c && b == d

-- May fail for extremely close a,b, but not obvious how to do correctly
less a b = toFloat a < toFloat b
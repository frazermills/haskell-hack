{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG

-- Function to draw the cat
drawCat :: Diagram B
drawCat = mconcat
  [
  -- hat
  drawWitchHat # scale 0.5 # translate (r2 (0, 0.3)),

    -- Eyes
  circle 0.13 # fc red
              # translate (r2 (-0.4, 0.25)),
  circle 0.13 # fc red
              # translate (r2 (-0.4, 0.25)),
  circle 0.13 # fc blue
              # translate (r2 (0.4, 0.25)),

  -- Nose
  triangle 0.26 # fc pink
                # rotateBy (1/2)
                # translate (r2 (0, 0)),

  -- Mouth
  vrule 0.4 # lw veryThin
            # translate (r2 (0, -0.2)),
  hrule 0.4 # lw veryThin
            # translate (r2 (0, -0.4)),

    -- Whiskers
  hrule 0.6 # lw veryThin
            # translate (r2 (-1, 0)),
  hrule 0.6 # lw veryThin
            # translate (r2 (1, 0)),
  hrule 0.6 # lw veryThin
            # translate (r2 (-1, -0.1)),
  hrule 0.6 # lw veryThin
            # translate (r2 (1, -0.1)),
  hrule 0.6 # lw veryThin
            # translate (r2 (-1, -0.2)),
  hrule 0.6 # lw veryThin
            # translate (r2 (1, -0.2)),

  -- Head
  circle 1  # fc lightgray
            # lw veryThick
            # lc pink
            # dashingG [0.2, 0.05] 0,

  -- Ears
  triangle 0.5  # scaleX 0.6
                # fc lightgray
                # translate (r2 (-0.6, 0.82))
                # rotateBy (1/20),
  triangle 0.5  # scaleX 0.6
                # fc lightgray
                # translate (r2 (0.6, 0.82))
                # rotateBy (-1/20)

  {-
  , batWithBaseballBat  # rotateBy (1/8)
                        # translate (r2 (3.2, 3))
                        # scale 1
-}


  , fairyWing # rotateBy (-1/8) # reflectY # translateX 0.8  # translateY 0.1 --right wing
  , fairyWing # rotateBy (-1/8) # reflectY # reflectX # translateX (-0.8)  # translateY 0.1 --right wing

  ]

drawWitchHat :: Diagram B
drawWitchHat =
   let -- Brim with jagged edges
        brimBase = roundedRect 6 1 0.5 # fc black # lw none # alignB
        brimCuts = [triangle 0.5 # rotateBy (1/12) # moveTo (p2 (x, 0.5))
                    | x <- [-2.5, -1.5 .. 2.5]]
        brim = mconcat (brimBase : brimCuts) # lw 0.5 # fc yellow # translate (r2 (0, 1))

        -- Cone of the hat
        conePoints = [p2 (0, 0), p2 (-2, -4), p2 (2, -4)]
        cone = fromVertices conePoints # closeLine # strokeLoop # fc yellow # lw 0.5 # translate (r2 (0, 6))

        -- Hat band and buckle
        band = rect 3.3 0.3 # fc green # lw none # moveTo (p2 (0, -1)) # translate (r2 (0, 4))
        buckleInner = roundedRect 0.4 0.2 0.1 # fc black # lw none # translate (r2 (0, 4))
        buckleOuter = roundedRect 0.6 0.4 0.1 # fc orange # lw none # translate (r2 (0, 4))
        buckle = (buckleInner <> buckleOuter) # moveTo (p2 (0, -1))

        -- Spider dangling from the tip
        spiderBody = circle 0.15 # fc black # lw none
        spiderLegs = mconcat [fromVertices [p2 (-0.1, 0), p2 (-0.3, -0.3)] # lw 0.1,
                             fromVertices [p2 (0.1, 0), p2 (0.3, -0.3)] # lw 0.1,
                             fromVertices [p2 (-0.1, 0), p2 (-0.3, 0.3)] # lw 0.1,
                             fromVertices [p2 (-0.1, 0), p2 (-0.3, 0.3)] # lw 0.1,
                             fromVertices [p2 (0, 0), p2 (0.4, 0)] # lw 0.1,
                             fromVertices [p2 (0, 0), p2 (-0.4, 0)] # lw 0.1
                             ]
        spider = (spiderBody <> spiderLegs) # moveTo (p2 (0, -4.5))
        spiderSilk = fromVertices [p2 (0, -4), p2 (0, -4.5)] # lw 0.05

        -- Assemble all parts of the hat
        hat = buckle `atop` band `atop` brim `atop` cone `atop` (spider `atop` spiderSilk) # translate (r2 (2.3, 5))

    in hat # frame 1

batWing1 :: Diagram B
batWing1 = mconcat
  [ strokeTrail (trailFromSegments [bezier3 (0.5 ^& 1.5) (1.5 ^& 2) (2.5 ^& 1)]) # lwG 0.1
  , strokeTrail (trailFromVertices [2.5 ^& 1, 2 ^& 0.5]) # lwG 0.1
  , strokeTrail (trailFromSegments [bezier3 (2 ^& 0.5) (1.75 ^& (-0.5)) (1.5 ^& (-1))]) # lwG 0.1
  , strokeTrail (trailFromSegments [bezier3 (1.5 ^& (-1)) (1 ^& (-1.5)) (0.5 ^& (-1))]) # lwG 0.1
  , strokeTrail (trailFromVertices [0.5 ^& (-1), origin]) # lwG 0.1
  ]

batWing2 :: Diagram B
batWing2 = mconcat
  [ strokeLoop (closeLine $ fromSegments [bezier3 (0.5 ^& 1.5) (1.5 ^& 2) (2.5 ^& 1)]) # fc black # lwG 0.1
  , strokeLoop (closeLine $ fromVertices [2.5 ^& 1, 2 ^& 0.5, origin]) # fc black # lwG 0.1
  , strokeLoop (closeLine $ fromSegments [bezier3 (2 ^& 0.5) (1.75 ^& (-0.5)) (1.5 ^& (-1))]) # fc black # lwG 0.1
  , strokeLoop (closeLine $ fromSegments [bezier3 (1.5 ^& (-1)) (1 ^& (-1.5)) (0.5 ^& (-1))]) # fc black # lwG 0.1
  , strokeLoop (closeLine $ fromVertices [0.5 ^& (-1), origin]) # fc black # lwG 0.1
  ]

boneSegment :: P2 Double -> P2 Double -> Diagram B
boneSegment start end =
  let bone = fromVertices [start, end] # lwG 0.1                   -- The bone line
      joint = circle 0.1 # fc white # moveTo start                 -- Joint at the start
      fang = triangle 0.3 # rotateBy (1/4) # moveTo end # fc black -- Fang at the end
  in mconcat [bone, joint, fang]

batWing3 :: Diagram B
batWing3 = mconcat
  [ boneSegment (p2 (0, 0)) (p2 (1.5, 1))
  , boneSegment (p2 (1.5, 1)) (p2 (2.5, 0.5))
  , boneSegment (p2 (2.5, 0.5)) (p2 (1.75, -0.5))
  , boneSegment (p2 (1.75, -0.5)) (p2 (1, -1))
  , boneSegment (p2 (1, -1)) (p2 (0.5, -1.5))
  ]

fairyWing :: Diagram B
fairyWing = mconcat
  [ strokeLoop (closeLine $ fromSegments [bezier3 (0.5 ^& 1.5) (1.5 ^& 2) (2.5 ^& 1)]) # fc lightblue # lwG 0.05 # opacity 1
  , strokeLoop (closeLine $ fromSegments [bezier3 (0.5 ^& 1.5) (0.2 ^& 0.5) (1.2 ^& 0)]) # fc lightblue # lwG 0.05 # opacity 0.5
  , strokeLoop (closeLine $ fromSegments [bezier3 (2.5 ^& 1) (3 ^& 0.5) (2 ^& (-1))]) # fc lightblue # lwG 0.05 # opacity 0.5
  , strokeLoop (closeLine $ fromSegments [bezier3 (1.2 ^& 0) (1.5 ^& (-0.5)) (0.5 ^& (-1))]) # fc lightblue # lwG 0.05 # opacity 0.5
  ]

wing :: Diagram B
wing = mconcat
  [ triangle 1 # scaleX 0.6 # rotateBy (1/8) # fc darkgray # translateX (-0.1),
    triangle 1 # scaleX 0.7 # rotateBy (1/12) # fc gray # translateX (-0.3),
    triangle 1 # scaleX 0.8 # rotateBy (1/16) # fc lightgray # translateX (-0.5)
  ]

body :: Diagram B
body = ellipseXY 0.3 0.5 # fc (sRGB24 50 0 50)

headAndEars :: Diagram B
headAndEars = mconcat
  [ circle 0.15 # fc (sRGB24 70 0 70) -- head
  , circle 0.05 # translateX (-0.1) # translateY 0.2 # fc (sRGB24 70 0 70) -- left ear
  , circle 0.05 # translateX 0.1 # translateY 0.2 # fc (sRGB24 70 0 70) -- right ear
  ]

baseballBat :: Diagram B
baseballBat = mconcat
  [ rect 0.12 0.8 # fc (sRGB24 205 133 63)
  , rect 0.2 0.5 # fc (sRGB24 222 184 135) # translateY 0.6
  , circle 0.08 # fc (sRGB24 139 69 19) # translateY (-0.4)
  ] # rotateBy (1/6)

-- Assemble the bat figure with a baseball bat
batWithBaseballBat :: Diagram B
batWithBaseballBat = mconcat
  [ baseballBat # translateX (-0.2) # translateY 0.2
  , body  -- Body
  , headAndEars # translateY 0.6
  , wing # rotateBy (5/8) # translateX (-0.8)
  , wing # rotateBy (5/8) # translateX (-0.8) # reflectX
  ]

-- Main function to render the cat to "cat.svg"
main :: IO ()
main = renderSVG "cat.svg" (mkWidth 200 :: SizeSpec V2 Double) (drawCat # centerXY # pad 1.5)

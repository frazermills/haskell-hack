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
  circle 0.13 # fc black
              # translate (r2 (-0.4, 0.25)),
  circle 0.13 # fc black
              # translate (r2 (-0.4, 0.25)),
  circle 0.13 # fc black
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
        cone = fromVertices conePoints # closeLine # strokeLoop # fc purple # lw 0.5 # translate (r2 (0, 6))

        -- Hat band and buckle
        band = rect 3.3 0.3 # fc black # lw none # moveTo (p2 (0, -1)) # translate (r2 (0, 4))
        buckleInner = roundedRect 0.4 0.2 0.1 # fc black # lw none # translate (r2 (0, 4))
        buckleOuter = roundedRect 0.6 0.4 0.1 # fc orange # lw none # translate (r2 (0, 4))
        buckle = (buckleInner <> buckleOuter) # moveTo (p2 (0, -1))

        -- Spider dangling from the tip
        spiderBody = circle 0.15 # fc black # lw none
        spiderLegs = mconcat [fromVertices [p2 (-0.1, 0), p2 (-0.3, -0.3)] # lw 0.05,
                             fromVertices [p2 (0.1, 0), p2 (0.3, -0.3)] # lw 0.05,
                             fromVertices [p2 (-0.1, 0), p2 (-0.3, 0.3)] # lw 0.05,
                             fromVertices [p2 (0.1, 0), p2 (0.3, 0.3)] # lw 0.05
                             ]
        spider = (spiderBody <> spiderLegs) # moveTo (p2 (0, -4.5))
        spiderSilk = fromVertices [p2 (0, -4), p2 (0, -4.5)] # lw 0.05

        -- Assemble all parts of the hat
        hat = buckle `atop` band `atop` brim `atop` cone `atop` (spider `atop` spiderSilk) # translate (r2 (2.3, 5))

    in hat # frame 1

-- Main function to render the cat to "cat.svg"
main :: IO ()
main = renderSVG "cat.svg" (mkWidth 200 :: SizeSpec V2 Double) (drawCat # centerXY # pad 1.5)

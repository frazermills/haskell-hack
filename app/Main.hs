{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG

-- Function to draw the cat
drawCat :: Diagram B
drawCat = mconcat
  [
    -- Eyes
  circle 0.13 # fc black # translate (r2 (-0.4, 0.25)),
  circle 0.13 # fc black # translate (r2 (0.4, 0.25)),

  -- Nose
  triangle 0.26 # fc pink # rotateBy (1/2) # translate (r2 (0, 0)),

  -- Mouth
  vrule 0.4 # lw veryThin # translate (r2 (0, -0.2)),
  hrule 0.4 # lw veryThin # translate (r2 (0, -0.4)),

    -- Whiskers
  hrule 0.6 # lw veryThin # translate (r2 (-1, 0)),
  hrule 0.6 # lw veryThin # translate (r2 (1, 0)),
  hrule 0.6 # lw veryThin # translate (r2 (-1, -0.1)),
  hrule 0.6 # lw veryThin # translate (r2 (1, -0.1)),
  hrule 0.6 # lw veryThin # translate (r2 (-1, -0.2)),
  hrule 0.6 # lw veryThin # translate (r2 (1, -0.2)),

  -- Head
  circle 1  # fc lightgray
            # lw veryThick
            # lc pink
            # dashingG [0.2, 0.05] 0,

  -- Ears
  triangle 0.5 # scaleX 0.6 # fc lightgray # translate (r2 (-0.6, 0.8)),
  triangle 0.5 # scaleX 0.6 # fc lightgray # translate (r2 (0.6, 0.8))
  ]

-- Main function to render the cat to "cat.svg"
main :: IO ()
main = renderSVG "cat.svg" (mkWidth 200 :: SizeSpec V2 Double) (drawCat # centerXY # pad 1.5)

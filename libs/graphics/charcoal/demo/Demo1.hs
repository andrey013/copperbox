

module Demo1 where

import Graphics.Charcoal.Extra
import Graphics.Charcoal.NumInstances
import Graphics.Charcoal.Picture



demo1 = drawIO $ fill 0


demo2 = drawIO $ nearest white black . sscaled 0.8 unitdisk


demo3 = drawIO $ nearest white black . regionToPicture white black vstrip

demo4 = drawIO $ nearest white black . sscaled 0.2 (regionToPicture white black checker)


demo5 = drawIO $ nearest white black . sscaled 0.1 (regionToPicture white black altRings)

demo6 = drawIO $ nearest white black . sscaled 0.1 (regionToPicture white black gasket)

demo7 = drawIO $ nearest white black . sscaled 0.5 
                   (regionToPicture white black unitcircle)

demo8 = do 
    drawIO $ nearest white black . (regionToPicture white black vhalfplane)
    drawIO $ nearest white black . applyTrafo (rotate $ pi / 4) 
                                              (regionToPicture white black hhalfplane)

demo9 = drawIO $ regionToPicture white black $ hhalfplane /\ (annulus 0.3 0.7)

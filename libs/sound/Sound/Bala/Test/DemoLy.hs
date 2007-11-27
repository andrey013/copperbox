


module Sound.Bala.Test.DemoLy where

import Sound.Bala.Format.LilyPond.LilyPondOutput

import Text.PrettyPrint.Leijen hiding (dot)

demo = putLy afro_twist_and_jive

afro_twist_and_jive :: Doc
afro_twist_and_jive = 
  header (title "Afro Twist & Jive")
  ## def fingerA
  ## def thumbA
  ## def fingerB
  ## def thumbB
  ## def fingerC
  ## def line1
  ## def line2
  ## score ((simult $ time 6 8 # tempo 4 112 # midiInstrument "marimba"
                    # braces ( use line1 # use line2))
           # midi empty  # layout empty)

line1 = var "lineOne" $ 
  (braces $ bar "|:" # poly [use fingerA, use thumbA] 
                     # poly [use fingerB, use thumbB]
                     # bar ":|" )

line2 = var "lineTwo" $ 
  (braces $ bar "|:" # poly [use fingerC, use thumbA] 
                     # poly [use fingerB, use thumbB]
                     # bar ":|" )
  
  
fingerA = var "fingerA" $ 
  (relative c'' $ braces $ r8 # dot c8 # dot c8 # dot d8 # c16 # tie)
    
thumbA = var "thumbA" $ 
  (relative c' $ braces $ dot c8 # dot e8 # dot f8 # f8 # r16)

fingerB = var "fingerB" $ 
  (relative c'' $ braces $ c8 # dot c8 # dot c8 # dot b8 # c16)
  
thumbB = var "thumbB" $ 
  (relative c' $ braces $ dot e8 # dot e8 # dot g_8 # f'8 # r16)
  
fingerC = var "fingerC" $ 
  (relative c'' $ braces $ r8 # dot c8 # dot e8 # dot d8 # c16 # tie)
    

  


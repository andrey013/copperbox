{-# OPTIONS -Wall #-}

module Symbols where

import Wumpus.Drawing.Chains
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Prelude hiding ( pi, product )

import System.Directory

main :: IO ()
main = do 
    createDirectoryIfMissing True "./out/"
    let pic1 = runCtxPictureU std_ctx symbols
    writeEPS "./out/symbols.eps" pic1
    writeSVG "./out/symbols.svg" pic1


std_ctx :: DrawingContext
std_ctx = set_font times_roman $ standardContext 12


-- Because the font changes, we draw the all the symbols in one
-- run and all the labels in a second run. This helps Wumpus-Core 
-- generate better PostScript as there are less changes to the 
-- /graphics state/.
--
symbols :: DCtxPicture
symbols = drawTracing $ do
    drawi_ $ localize (set_font symbol) $ 
               chn (map sdraw all_letters) `at` start
    drawi_ $ chn (map ldraw all_letters) `at` start
  where
    chn             = tableDown 30 (100,20) 
    start           = P2 0 (30*20)
    sdraw (s,_)     = textline s
    ldraw (_,name)  = moveStart (displaceH 16) (textline name)


all_letters :: [(String, String)]
all_letters = 
    [ ("&Alpha;",               "Alpha") 
    , ("&Beta;",                "Beta")
    , ("&Chi;",                 "Chi")
    , ("&Delta;",               "Delta")
    , ("&Epsilon;",             "Epsilon")
    , ("&Eta;",                 "Eta")
    , ("&Euro;",                "Euro")
    , ("&Gamma;",               "Gamma")
    , ("&Ifraktur;",            "Ifraktur")
    , ("&Iota;",                "Iota")
    , ("&Kappa;",               "Kappa")
    , ("&Lambda;",              "Lambda")
    , ("&Mu;",                  "Mu")
    , ("&Nu;",                  "Nu")
    , ("&Omega;",               "Omega")
    , ("&Omicron;",             "Omicron")
    , ("&Phi;",                 "Phi")
    , ("&Pi;",                  "Pi")
    , ("&Psi;",                 "Psi")
    , ("&Rfraktur;",            "Rfraktur")
    , ("&Rho;",                 "Rho")
    , ("&Sigma;",               "Sigma")
    , ("&Tau;",                 "Tau")
    , ("&Theta;",               "Theta")
    , ("&Upsilon;",             "Upsilon")
    , ("&Upsilon1;",            "Upsilon1")
    , ("&Xi;",                  "Xi")
    , ("&Zeta;",                "Zeta")
    , ("&aleph;",               "aleph")
    , ("&alpha;",               "alpha")
    , ("&ampersand;",           "ampersand")
    , ("&angle;",               "angle")
    , ("&angleleft;",           "angleleft")
    , ("&angleright;",          "angleright")
    , ("&approxequal;",         "approxequal")

    -- 
    , ("&arrowboth;",           "arrowboth") 
    , ("&arrowdblboth;",        "arrowdblboth")
    , ("&arrowdbldown;",        "arrowdbldown")
    , ("&arrowdblleft;",        "arrowdblleft")
    , ("&arrowdblright;",       "arrowdblright")
    , ("&arrowdblup;",          "arrowdblup")
    , ("&arrowdown;",           "arrowdown")
    , ("&arrowleft;",           "arrowleft")
    , ("&arrowright;",          "arrowright")
    , ("&arrowup;",             "arrowup")
    , ("&asteriskmath;",        "asteriskmath")
    , ("&bar;",                 "bar")
    , ("&beta;",                "beta")
    , ("&braceleft;",           "braceleft")
    , ("&braceright;",          "braceright")
    , ("&bracketleft;",         "bracketleft")
    , ("&bracketright;",        "bracketright")
    , ("&bullet;",              "bullet")
    , ("&carriagereturn;",      "carriagereturn")
    , ("&chi;",                 "chi")

    --
    , ("&circlemultiply;",      "circlemultiply") 
    , ("&circleplus;",          "circleplus")
    , ("&club;",                "club")
    , ("&colon;",               "colon")
    , ("&comma;",               "comma")
    , ("&congruent;",           "congruent")
    , ("&copyrightsans;",       "copyrightsans")
    , ("&copyrightserif;",      "copyrightserif")
    , ("&degree;",              "degree")
    , ("&delta;",               "delta")
    , ("&diamond;",             "diamond")
    , ("&divide;",              "divide")
    , ("&dotmath;",             "dotmath")
    , ("&eight;",               "eight")
    , ("&element;",             "element")
    , ("&ellipsis;",            "ellipsis")
    , ("&emptyset;",            "emptyset")
    , ("&epsilon;",             "epsilon")
    , ("&equal;",               "equal")
    , ("&equivalence;",         "equivalence")
    , ("&eta;",                 "eta")
    , ("&exclam;",              "exclam")
    , ("&existential;",         "existential")
    , ("&five;",                "five")
    , ("&florin;",              "florin")
    , ("&four;",                "four")
    , ("&fraction;",            "fraction")
    , ("&gamma;",               "gamma")
    , ("&gradient;",            "gradient")
    , ("&greater;",             "greater")
    , ("&greaterequal;",        "greaterequal")
    , ("&heart;",               "heart")
    , ("&infinity;",            "infinity")
    , ("&integral;",            "integral")

    -- 
    , ("&intersection;",        "intersection")
    , ("&iota;",                "iota")
    , ("&kappa;",               "kappa")
    , ("&lambda;",              "lambda")
    , ("&less;",                "less")
    , ("&lessequal;",           "lessequal")
    , ("&logicaland;",          "logicaland")
    , ("&logicalnot;",          "logicalnot")
    , ("&logicalor;",           "logicalor")
    , ("&lozenge;",             "lozenge")
    , ("&minus;",               "minus")
    , ("&minute;",              "minute")
    , ("&mu;",                  "mu")
    , ("&multiply;",            "multiply")
    , ("&nine;",                "nine")
    , ("&notelement;",          "notelement")
    , ("&notequal;",            "notequal")
    , ("&notsubset;",           "notsubset")
    , ("&nu;",                  "nu")
    , ("&numbersign;",          "numbersign")
    , ("&omega;",               "omega")
    , ("&omega1;",              "omega1")
    , ("&omicron;",             "omicron")
    , ("&one;",                 "one")
    , ("&parenleft;",           "parenleft")
    , ("&parenright;",          "parenright")

    --
    , ("&partialdiff;",         "partialdiff")
    , ("&percent;",             "percent")
    , ("&period;",              "period")
    , ("&perpendicular;",       "perpendicular")
    , ("&phi;",                 "phi")
    , ("&phi1;",                "phi1")
    , ("&pi;",                  "pi")
    , ("&plus;",                "plus")
    , ("&plusminus;",           "plusminus")
    , ("&product;",             "product")
    , ("&propersubset;",        "propersubset")
    , ("&propersuperset;",      "propersuperset")
    , ("&proportional;",        "proportional")
    , ("&psi;",                 "psi")
    , ("&question;",            "question")
    , ("&radical;",             "radical")
    , ("&radicalex;",           "radicalex")
    , ("&reflexsubset;",        "reflexsubset")
    , ("&reflexsuperset;",      "reflexsuperset")
    , ("&registersans;",        "registersans")
    , ("&registerserif;",       "registerserif")
    , ("&rho;",                 "rho")
    
    -- 
    , ("&second;",              "second")
    , ("&semicolon;",           "semicolon")
    , ("&seven;",               "seven")
    , ("&sigma;",               "sigma")
    , ("&sigma1;",              "sigma1")
    , ("&similar;",             "similar")
    , ("&six;",                 "six")
    , ("&slash;",               "slash")
    , ("&space;",               "space")
    , ("&spade;",               "spade")
    , ("&suchthat;",            "suchthat")
    , ("&summation;",           "summation")
    , ("&tau;",                 "tau")
    , ("&therefore;",           "therefore")
    , ("&theta;",               "theta")
    , ("&theta1;",              "theta1")
    , ("&three;",               "three")
    , ("&trademarksans;",       "trademarksans")
    , ("&trademarkserif;",      "trademarkserif")
    , ("&two;",                 "two")
    , ("&underscore;",          "underscore")
    , ("&union;",               "union")
    , ("&universal;",           "universal")
    , ("&upsilon;",             "upsilon")
    , ("&weierstrass;",         "weierstrass")
    , ("&xi;",                  "xi")
    , ("&zero;",                "zero")
    , ("&zeta;",                "zeta")
    ]


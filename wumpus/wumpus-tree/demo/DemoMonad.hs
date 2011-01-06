

module DemoMonad where

import FontLoaderUtils

import Wumpus.Tree
import Wumpus.Tree.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Afm
import Wumpus.Basic.System.FontLoader.GhostScript
import Wumpus.Drawing.Dots.AnchorDots
import Wumpus.Drawing.Colour.SVGColours
import Wumpus.Drawing.Text.SafeFonts

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative
import Control.Monad
import qualified Data.IntMap as IntMap
import Data.Monoid
import Data.Tree

import System.Directory



data NodeId a = NodeId Int
              | RegularNode a
  deriving (Eq)

type ZNodeId = NodeId ()

type NodeDrawRefs u = IntMap.IntMap (TreeNode u)

data St u = St
      { uid        :: Int
      , node_refs  :: NodeDrawRefs u
      }

zeroSt :: St u
zeroSt = St { uid = 0, node_refs = mempty }
            


newtype TreeDrawing u a = TreeDrawing { getTreeDrawing :: St u -> (a, St u) }

instance Functor (TreeDrawing u) where
  fmap f ma = TreeDrawing $ \s -> let (a,s1) = getTreeDrawing ma s in (f a, s1)

instance Applicative (TreeDrawing u) where
  pure a    = TreeDrawing $ \s -> (a,s)
  mf <*> ma = TreeDrawing $ \s -> let (f,s1) = getTreeDrawing mf s
                                      (a,s2) = getTreeDrawing ma s1
                                  in (f a,s2)

instance Monad (TreeDrawing u) where
  return a  = TreeDrawing $ \s -> (a,s)
  ma >>= k  = TreeDrawing $ \s -> let (a,s1) = getTreeDrawing ma s 
                                  in getTreeDrawing (k a) s1 



nodeId :: TreeNode u -> TreeDrawing u (NodeId a)
nodeId drawF = TreeDrawing $ \(St uid nodes) -> 
                 let nodes' = IntMap.insert uid drawF nodes
                 in (NodeId uid, St (uid+1) nodes')


type TreeSpec a = Tree (NodeId a)
type ZTreeSpec  = TreeSpec ()


runTreeDrawing :: (Real u, Floating u, FromPtSize u)
               => (a -> TreeNode u) -> TreeDrawing u (TreeSpec a) -> Tree (TreeNode u)
runTreeDrawing regDrawF ma = 
    let (a,s) = getTreeDrawing ma zeroSt in postRun regDrawF (a, node_refs s)


-- With careful building no NodeIds should refer to uninstantiated
-- nodes, however we still need to do something about 
-- IntMap.lookup failure.
--
postRun :: (Real u, Floating u, FromPtSize u)
        => (a -> TreeNode u) -> (TreeSpec a,NodeDrawRefs u) -> Tree (TreeNode u)
postRun regDrawF (tree1,table) = fmap changeNode tree1
  where
    changeNode (RegularNode a)  = regDrawF a
    changeNode (NodeId ix)      = maybe fk id $ IntMap.lookup ix table 
    
    fk                          = dotText "Error missing node"
 




branch :: NodeId a -> [TreeSpec a] -> TreeSpec a
branch uid kids = Node uid kids

label :: a -> NodeId a 
label a = RegularNode a

-- | Default /branch/ - has children.
--
zbranch :: [ZTreeSpec] -> ZTreeSpec
zbranch kids = Node (RegularNode ()) kids 

leaf :: NodeId a -> TreeSpec a
leaf uid = Node uid []

-- | Default /leaf/ - tree node with no children.
--
zleaf :: ZTreeSpec
zleaf = Node (RegularNode ()) []


tree1 :: TreeDrawing u (TreeSpec Char)
tree1 = return $ 
    branch (label 'A') [branch (label 'B') bs, branch (label 'F') gs]
  where
    bs = [leaf $ label 'C', leaf $ label 'D', leaf $ label 'E']
    gs = [branch (label 'G') [ leaf $ label 'H'
                             , leaf $ label 'I'
                             , leaf $ label 'J' ] ]


tree_drawing1 :: DTreePicture
tree_drawing1 = drawScaledTree2 (uniformScaling 30) $ 
                  runTreeDrawing charNode (tree1 :: TreeDrawing Double (TreeSpec Char ))



tree2 :: (Real u, Floating u, FromPtSize u) => TreeDrawing u ZTreeSpec
tree2 = do
    special <- nodeId $ dotText "root"   
    return $ 
      branch special [zbranch bs, zleaf, zbranch gs]
  where
    bs = [zleaf, zleaf, zleaf]
    gs = [zleaf, zleaf, zleaf]



tree_drawing2 :: DTreePicture
tree_drawing2 = drawScaledTree2 (uniformScaling 60) $ 
                  runTreeDrawing (diskNode red) 
                                 (tree2 :: TreeDrawing Double ZTreeSpec)


main :: IO ()
main = do
    (mb_gs, mb_afm) <- processCmdLine default_font_loader_help
    createDirectoryIfMissing True "./out/"
    case (mb_gs, mb_afm) of       
      (Just dir, _) -> do { putStrLn "Using GhostScript metrics..."
                          ; (metrics,msgs) <- loadGSMetrics  dir ["Times-Roman"]
                          ; mapM_ putStrLn msgs
                          ; makePictures metrics
                          }
      (_, Just dir) -> do { putStrLn "Using AFM v4.1 metrics..."
                          ; (metrics,msgs) <- loadAfmMetrics dir ["Times-Roman"]
                          ; mapM_ putStrLn msgs
                          ; makePictures metrics
                          }
      _             -> putStrLn default_font_loader_help




makePictures :: GlyphMetrics -> IO ()
makePictures base_metrics = do 
    let pic1 = runCtxPictureU (makeCtx 18 base_metrics) tree_drawing1
    writeEPS "./out/mon_tree01.eps"  pic1
    writeSVG "./out/mon_tree01.svg"  pic1

    let pic2 = runCtxPictureU (makeCtx 24 base_metrics) tree_drawing2
    writeEPS "./out/mon_tree02.eps"  pic2
    writeSVG "./out/mon_tree02.svg"  pic2


makeCtx :: FontSize -> GlyphMetrics -> DrawingContext
makeCtx sz m = fontFace times_roman $ metricsContext sz m


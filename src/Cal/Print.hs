-- | Non-interactive rendering of the current view.
module Cal.Print (renderPrint) where

import Cal.Doc (renderAnsi)
import Cal.Model.Common (Env, envSettings)
import Cal.Types (Settings (..), defaultCursor)
import Cal.View (faceDoc)
import Cal.View.Types (mkRenderCtx, printBlock)
import Data.Text (Text)
import qualified Data.Text as T

-- | The rendered view at the given width, without a trailing newline.
renderPrint :: Env -> Int -> Either Text Text
renderPrint env w = case mkRenderCtx env w False of
  Nothing -> Left ("hledger cal needs at least 80 columns (have " <> T.pack (show w) <> ")")
  Just ctx -> Right (renderAnsi (sColor (envSettings env)) (printBlock ctx (faceDoc ctx env defaultCursor)))

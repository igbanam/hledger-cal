-- | Dispatch from the settings' face to the matching model and view.
module Cal.View (faceDoc) where

import Cal.Model.Common (Env, envSettings)
import Cal.Model.Commodities (commoditiesModel)
import Cal.Model.Month (monthModel)
import Cal.Model.Quarter (quarterModel)
import Cal.Model.Week (weekModel)
import Cal.Model.Year (yearModel)
import Cal.Types (Cursor, Face (..), Settings (..))
import Cal.View.Commodities (commoditiesDoc)
import Cal.View.Month (monthDoc)
import Cal.View.Quarter (quarterDoc)
import Cal.View.Types (RenderCtx, ViewDoc)
import Cal.View.Week (weekDoc)
import Cal.View.Year (yearDoc)

faceDoc :: RenderCtx -> Env -> Cursor -> ViewDoc
faceDoc ctx env cur = case sFace (envSettings env) of
  FaceYear -> yearDoc ctx (yearModel env)
  FaceQuarter -> quarterDoc ctx (quarterModel env)
  FaceMonth -> monthDoc ctx (monthModel env)
  FaceWeek -> weekDoc ctx (weekModel env cur)
  FaceCommodities -> commoditiesDoc ctx (commoditiesModel env cur)

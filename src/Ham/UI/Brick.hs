{-# LANGUAGE OverloadedStrings, RankNTypes, TupleSections #-}


module Ham.UI.Brick
  (
    emptyAppState
  , AppMode(..)
  , AppState(..)
  , AppResource
  , app
  )
where

import Ham.Log
import Ham.Data
import qualified Ham.CAT as CAT
import Ham.CAT.ElecraftKX2
import Ham.CAT.YaesuFT891

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

import Data.Monoid ((<>))
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Foldable
import Data.List (foldl')
import qualified Data.Text as Text
import qualified Data.Sequence as S
import qualified Data.Vector as V

import Brick
import Brick.Forms
import Brick.Widgets.List
import Brick.Widgets.Border
import Brick.Widgets.Dialog
import qualified Brick.Widgets.Edit as E
import qualified Brick.Focus as F
import qualified Brick.Types as T
import Brick.Main

import qualified Brick.Widgets.Border.Style
import Graphics.Vty.Attributes (defAttr, blue)
import Graphics.Vty.Input.Events
import qualified Graphics.Vty as V

import Lens.Micro



-- | Application state for the 'Brick' UI.
data AppState = AppState {
  logState :: LogState,             -- ^ State for the Hamlog monad.
  logConfig :: LogConfig,           -- ^ Configuration for the Hamlog monad.
  catConfig :: CAT.CATConfig,       -- ^ Configuration for the CAT interface.
  catState  :: CAT.CATState,
  qsoList :: List AppResource Qso,  -- ^ List of contacts to display
  qsoForm :: Form Qso HamlogEvent AppResource, -- ^ Form to enter new contacts
  focusRing :: F.FocusRing AppResource, -- ^ Focus ring to use
  appMode :: AppMode,                   -- ^ Current mode of the app
  selectedQsoIndex :: Int,              -- ^ Index of the currently selected contact
  statusText :: [Text],                 -- ^ Text to display in the status line
  duplicateQsos :: S.Seq Duplicate      -- ^ Any potential duplicates found.
  }


-- | Default AppState.
emptyAppState :: AppState
emptyAppState =
  AppState { logState = emptyLogState,
             logConfig = defaultConfig,
             catConfig = CAT.defaultConfig { CAT.catPort = "/dev/ttyUSB0" },
             catState = CAT.defaultState { CAT.stateInterface = yaesuFT891 },
             qsoList = list LogList V.empty 1,
             qsoForm = newForm [] emptyQso,
             focusRing = lappDefaultFocusRing,
             appMode = AppModeList,
             selectedQsoIndex = 0,
             statusText = [],
             duplicateQsos = S.Empty
           }



data HamlogEvent

-- | Resource type to index widgets
data AppResource = LogList |
                   LogQso |
                   LogQsoTimeStart |
                   LogQsoTimeEnd |
                   LogQsoFrequency |
                   LogQsoMode |
                   LogQsoCallsign |
                   LogQsoLocation |
                   LogQsoRST |
                   LogQsoExchange |
                   LogQsoMyCallsign |
                   LogQsoMyLocation |
                   LogQsoSentRST |
                   LogQsoSentExchange |
                   LogQsoNotes
  deriving (Eq, Ord, Show)


-- | Run a 'HamLog' action given the AppState, in the EventM monad.
hamLog :: AppState -> HamLog a -> EventM AppResource (a, AppState)
hamLog s act = do
  (a, ls, _) <- liftIO $ runHamLog (logConfig s) (logState s) $ act
  let s' = s { logState = ls }
  return (a, s')


-- | Run a CAT action.
cat :: AppState -> CAT.CAT (EventM AppResource) a -> EventM AppResource (a, AppState)
cat s act = do
  (a, w) <- CAT.runCAT (catConfig s) (catState s) act
  liftIO $ mapM putStrLn w
  return (a, s)


-- | Default focus ring. Empty.
lappDefaultFocusRing :: F.FocusRing AppResource
lappDefaultFocusRing = F.focusRing []


-- | Focus ring for the QSO edit widget.
lappQsoFocusRing :: AppState -> F.FocusRing AppResource
lappQsoFocusRing s = formFocus $ qsoForm s


-- | Add a new QSO at the beginning of the list and go to edit mode immediately.
lappNewQso :: AppState -> EventM AppResource AppState
lappNewQso s = do
  (q, s') <- hamLog s newQsoNow
  s'' <- lupdateQsoList 0 s' >>= lupdateQsoForm

  -- Edit the new QSO (at index 0)
  return $ s'' { appMode = AppModeQso, focusRing = lappQsoFocusRing s'', selectedQsoIndex = 0 }




-- | Given a QSO, create a form for editing it.
mkQsoForm :: Qso -> QsoDefaults -> Form Qso HamlogEvent AppResource
mkQsoForm q q_def = newForm fieldStates q
  where
    timeStart = f _qsoDefaultTimeStart $ label "Start:" @@= editShowableField qsoTimeStart LogQsoTimeStart
    timeEnd   = f _qsoDefaultTimeEnd   $ label "End:"   @@= editShowableField qsoTimeEnd LogQsoTimeEnd
    f d s = case d q_def of
      FixedValue _ -> []
      DefaultValue _ -> [s]
    fieldStates = concat [
                          f _qsoDefaultCallsign     $ label "Callsign:"     @@= editTextField qsoCallsign LogQsoCallsign (Just 1),
                          f _qsoDefaultLocation     $ label "Location:"     @@= editTextField qsoLocation LogQsoLocation (Just 1),
                          f _qsoDefaultRST          $ label "RST received:" @@= editShowableField qsoRST LogQsoRST,
                          f _qsoDefaultExchange     $ label "Exchg recvd:"  @@= editTextField qsoExchange LogQsoExchange (Just 1),
                          f _qsoDefaultSentRST      $ label "RST sent:"     @@= editShowableField qsoSentRST LogQsoSentRST,
                          f _qsoDefaultSentExchange $ label "Exchg sent:"   @@= editTextField qsoSentExchange LogQsoSentExchange (Just 1),
                          f _qsoDefaultFrequency    $ label "Frequency:"    @@= editShowableField qsoFrequency LogQsoFrequency,
                          f _qsoDefaultMode         $ label "Mode (CW,PH,FM,RY):" @@= editShowableField qsoMode LogQsoMode,
                          f _qsoDefaultSentCallsign $ label "My callsign:"  @@= editTextField qsoSentCallsign LogQsoMyCallsign (Just 1),
                          f _qsoDefaultSentLocation $ label "My location:"  @@= editTextField qsoSentLocation LogQsoMyLocation (Just 1),
                          timeStart,
                          timeEnd,
                          f _qsoDefaultNotes        $ label "Notes:"        @@= editTextField qsoNotes LogQsoNotes Nothing]
    label s w = -- padBottom (Pad 1) $
                (vLimit 1 $ hLimit 20 $ str s <+> fill ' ') <+> w


-- | Create a form for editing the currently selected QSO.
mkSelectedQsoForm :: AppState -> Form Qso HamlogEvent AppResource
mkSelectedQsoForm s = qsoForm
  where
    q_def   = _configQsoDefaults $ logConfig s
    qsoForm = mkQsoForm selectedQso q_def
    selectedQso = maybe emptyQso id $ snd <$> listSelectedElement (qsoList s)


-- | Selected QSO. If none is selected, the empty QSO is returned.
lselectedQso :: AppState -> Qso
lselectedQso s = maybe emptyQso id $ snd <$> listSelectedElement (qsoList s)


-- | Drawing function. Draws depending on current AppMode.
lappDraw :: AppState -> [Widget AppResource]
lappDraw s = case (appMode s) of
  AppModeQuestion _ dlg _ _ -> [ renderDialog dlg $ str "", mainView ]
  _                         -> [ mainView ]
  where
    mainView = ((renderListWidget <=> lappStatusTextWidget s) <+> (qsoWidget <=> lappHelp s <=> appInfoWidget s)) <=> statusWidget
    renderListWidget = border $ hLimit 30 $ renderList lrenderLogListItem True l
    l = qsoList s
    qsoWidget = border $ {-padTop (Pad 0) $-} renderForm $ qsoForm s
    statusWidget = lappStatusWidget s


lappStatusTextWidget :: AppState -> Widget AppResource
lappStatusTextWidget s = str t
  where t = unlines $ map T.unpack $ statusText s


-- | Information widget. Shows dupes.
appInfoWidget :: AppState -> Widget AppResource
appInfoWidget s = info
  where info = padTop (Pad 1) $ borderWithLabel (str "Info") body
        body = case (appMode s) of
          -- AppModeList -> str ""
          _ ->
            let
              dupes = duplicateQsos s
              dupeToText dupe = show dupe

            in case dupes of
              S.Empty -> str "No info."
              _ -> vBox $ toList $ fmap f dupes
                where f dupe = if (duplicateBand dupe) && (duplicateMode dupe)
                               then withAttr "dupeWarn" $ str $ dupeToText dupe
                               else str $ dupeToText dupe
              -- _ -> str . T.unpack $ "Possible duplicates:\n" <>
              --           T.unlines (map dupeToText $ toList dupes)

          -- _ -> str ""



-- | Rendering a list item in the log list.
lrenderLogListItem :: Bool -> Qso -> Widget AppResource
lrenderLogListItem True  q = withAttr ("qsoList" <> "selected") $ str $ lrenderLogListItem_str q
lrenderLogListItem False q = withAttr ("qsoList" <> "normal")   $ str $ lrenderLogListItem_str q
lrenderLogListItem_str :: Qso -> String
lrenderLogListItem_str q = time_date ++ " " ++ call ++ " " ++ bd ++ " " ++ mode
  where time_date = formatTime  defaultTimeLocale "%F %R" (_qsoTimeStart q)
        call      = Text.unpack (_qsoCallsign q)
        bd        = show $ band $ _qsoFrequency q
        mode      = show $ _qsoMode q

-- | Update the UI list widget with the current log list in the state.
lupdateQsoList :: Int -> AppState -> EventM AppResource AppState
lupdateQsoList qso_index s = return $ s { qsoList = l }
  where
    l  = listMoveTo qso_index $ list LogList ll 1
    ll = V.fromList $ (foldr (:) [] . _logQsos . _stateLog . logState) s


-- | Update the QSO widget with the currently selected QSO in the list.
lupdateQsoForm :: AppState -> EventM AppResource AppState
lupdateQsoForm s = return $ s { qsoForm = mkSelectedQsoForm s }


-- | Handler for AppModeQuestion; this is for asking the user a yes/no question.
lhandleEvent_question :: AppState -> BrickEvent AppResource HamlogEvent -> EventM AppResource (Next AppState)
lhandleEvent_question s ev =
  case appMode s of

    AppModeQuestion prev dlg yes no ->

      case ev of

        VtyEvent (EvKey KEnter []) -> do
          let msel = dialogSelection dlg
          let s' = s { appMode = prev }
          case msel of
            Just sel -> continue =<< if sel then (yes s') else (no s')

        VtyEvent (EvKey (KChar 'y') []) -> do
          let s' = s { appMode = prev }
          continue =<< yes s'

        VtyEvent (EvKey (KChar 'n') []) -> do
          let s' = s { appMode = prev }
          continue =<< no s'

        VtyEvent (EvKey KEsc []) -> do
          let s' = s { appMode = prev }
          continue =<< no s'

        VtyEvent e -> do
          dlg' <- handleDialogEvent e dlg
          let s' = s { appMode = AppModeQuestion prev dlg' yes no }
          continue s'
        _ -> continue s

    _ -> continue s


-- | List mode. This is the default, main mode of the application.
lhandleEvent_list :: AppState -> BrickEvent AppResource HamlogEvent -> EventM AppResource (Next AppState)
lhandleEvent_list s ev =
  case appMode s of

    AppModeList ->
      -- This mode is editing the log list.

      case ev of

        VtyEvent (EvKey (KChar 'q') [])  -> do
          (_, s') <- hamLog s writeLog
          halt s'

        VtyEvent (EvKey (KChar 'n') [])  -> do
          (mf, mm) <- fst <$> cat s (do { f <- CAT.catFrequency; m <- CAT.catMode; return (f,m) } )
          let
            updated_qso_defaults = qso_defaults {
              _qsoDefaultFrequency = case _qsoDefaultFrequency qso_defaults of
                                       DefaultValue f -> DefaultValue (maybe f id mf)
                                       a              -> a,
              _qsoDefaultMode = case _qsoDefaultMode qso_defaults of
                                  DefaultValue m -> DefaultValue (maybe m id mm)
                                  a              -> a }
            --, _qsoDefaultMode = case _qsoDefaultMode qso_defaults of
            --                      DefaultValue _ -> DefaultValue (_qsoMode f) }
            qso_defaults         = _configQsoDefaults $ logConfig s
            updated_config       = (logConfig s) { _configQsoDefaults = updated_qso_defaults }
            s'                   = s { logConfig = updated_config }
          continue =<< lappNewQso s'

        VtyEvent (EvKey KBS [])         -> do
          let dlg = dialog (Just "Delete entry?") (Just (1, [("Yes", True), ("No", False)])) 25
              s' = s { appMode = AppModeQuestion AppModeList dlg yes no }
              yes ss = maybe
                         (return ss)
                         (\i -> snd <$> hamLog ss (deleteQso i) >>=
                                        lupdateQsoList i >>=
                                        lupdateQsoForm >>= \ss' ->
                                        lcheckDupes (lselectedQso ss') ss')  -- Check for dupes again after deleting an element.
                         mi
              no = return
              mi = listSelected $ qsoList s'
          continue s'

        VtyEvent (EvKey (KChar 's') []) -> continue $ s { appMode = AppModeSort AppModeList }

        VtyEvent (EvKey (KChar 'w') []) -> snd <$> hamLog s writeLog >>= continue

        VtyEvent (EvKey (KChar '\t') []) -> do
          let mi = listSelected $ qsoList s
          case mi of
            Just i -> continue $ s { appMode = AppModeQso, focusRing = lappQsoFocusRing s, selectedQsoIndex = i }
            Nothing -> continue s
        VtyEvent e                       -> do
          s' <- handleListEvent e (qsoList s) >>= \l -> return (s { qsoList = l })
          continue =<< lupdateQsoForm s'
        _ -> continue s

    _ -> continue s


lhandleEvent_qso :: AppState -> BrickEvent AppResource HamlogEvent -> EventM AppResource (Next AppState)
lhandleEvent_qso s ev =
  case (appMode s) of

    AppModeQso ->
      -- This mode is editing the current QSO entry.

      case ev of

        VtyEvent (EvKey KUp []) -> do
          let focusring = lappQsoFocusRing s
              m_current_focus = F.focusGetCurrent focusring

          case m_current_focus of
            Just LogQsoNotes ->
              handleFormEvent ev (qsoForm s) >>= \f ->
              continue $ s { qsoForm = f, focusRing = lappQsoFocusRing s }
            Just _ -> lhandleEvent_qso s (VtyEvent (EvKey KBackTab []))

        VtyEvent (EvKey KDown []) -> do
          let focusring = lappQsoFocusRing s
              m_current_focus = F.focusGetCurrent focusring

          case m_current_focus of
            Just LogQsoNotes ->
              handleFormEvent ev (qsoForm s) >>= \f ->
              continue $ s { qsoForm = f, focusRing = lappQsoFocusRing s }
              --return (s { qsoForm = f }) >>= \ss ->
              --return (ss { focusRing = lappQsoFocusRing ss }) >>= continue
            Just _ -> lhandleEvent_qso s (VtyEvent (EvKey (KChar '\t') []))

        VtyEvent (EvKey (KChar 'l') [MCtrl]) -> do
          let f = formState $ qsoForm s
          let s' = s { statusText = ["[Retrieving...]"] }
          (name, _) <- hamLog s $ lookupFccName (_qsoCallsign f)
          continue $ s { statusText = [name] }


        VtyEvent (EvKey (KChar 't') [MCtrl]) -> do
          let f = formState $ qsoForm s
              s' = s { statusText = ["Time updated"] }
              i = selectedQsoIndex s'
          (mf', s'') <- hamLog s' $ do
            currentUtcTime >>= \t -> updateQso i (f { _qsoTimeStart = t, _qsoTimeEnd = t })
            a <- getQsoSeq
            return (a S.!? i)
          -- FIXME: I need to update the form entries, not the qso directly.

          lupdateQsoForm s'' >>= continue -- FIXME: The time is not updated in the form, and then set to the old time when the form is left.


        VtyEvent (EvKey KEsc []) -> do
          -- Modify the qso currently under edit and go back to list mode.
          let f = formState $ qsoForm s
              i = selectedQsoIndex s
              -- Also modify the a subset of the configured default values to the ones we set last, if it is not a FixedValue.
              updated_qso_defaults = qso_defaults { _qsoDefaultFrequency = case _qsoDefaultFrequency qso_defaults of
                                                                             DefaultValue _ -> DefaultValue (_qsoFrequency f)
                                                                             a              -> a
                                                  , _qsoDefaultMode = case _qsoDefaultMode qso_defaults of
                                                                        DefaultValue _ -> DefaultValue (_qsoMode f) }
              qso_defaults         = _configQsoDefaults $ logConfig s
              updated_config       = (logConfig s) { _configQsoDefaults = updated_qso_defaults }

          -- Check for duplicates using a Qso type object.
          -- FIXME: Add a level of duplicate: Met on another band, on the same band but different mode,
          -- met on the same band with the same mode.
          -- (dupes, s') <- hamLog s $ findDuplicateQsos f
          s' <- (hamLog s $ updateQso i f) >>= lcheckDupes f . snd
          -- s'' <- lcheckDupes f s'

          let s'' = s' { appMode = AppModeList,
                         focusRing = lappDefaultFocusRing,
                         logConfig = updated_config,
                         selectedQsoIndex = -1 }

          -- FIXME: Do this more efficiently than updating the whole list.
          continue =<< lupdateQsoList i s''

        _ -> do
          s' <- handleFormEvent ev (qsoForm s) >>= \f ->
                --return s { qsoForm = f, focusRing = lappQsoFocusRing s }
                return (s { qsoForm = f }) >>= \ss ->
                return (ss { focusRing = lappQsoFocusRing ss }) -- NOTE: This is apparently necessary to get the text cursor to show.
          continue s'
    _ -> continue s



lcheckDupes :: Qso -> AppState -> EventM AppResource AppState
lcheckDupes qso s = do
  (dupes, s') <- hamLog s $ findDuplicateQsos qso
  return $ s' { duplicateQsos = dupes }



data QsoSortKey = QsoSortTime
                | QsoSortCallsign
                | QsoSortLocation


lsetAppMode :: AppMode -> AppState -> EventM AppResource AppState
lsetAppMode m s = return $ s { appMode = m }


lhandleEvent_sortQsos :: AppState -> BrickEvent AppResource HamlogEvent -> EventM AppResource (Next AppState)
lhandleEvent_sortQsos s ev =
  case appMode s of
    AppModeSort prev -> do
      ms' <- case ev of
               VtyEvent (EvKey (KChar 't') []) -> snd <$> hamLog s (sortLog _qsoTimeStart) >>= lsetAppMode prev >>= return . Just
               VtyEvent (EvKey (KChar 'c') []) -> snd <$> hamLog s (sortLog _qsoCallsign)  >>= lsetAppMode prev >>= return . Just
               VtyEvent (EvKey (KChar 'l') []) -> snd <$> hamLog s (sortLog _qsoLocation)  >>= lsetAppMode prev >>= return . Just
               _ -> return Nothing
      maybe (continue s) (\s' -> lupdateQsoList 0 s' >>= lupdateQsoForm >>= continue) ms'
    _ -> continue s


lappStatusWidget :: AppState -> Widget AppResource
lappStatusWidget _ = str $ "(C) Copyright KM6THJ, 2018."


-- | Help text, depending on mode.
lappHelp :: AppState -> Widget AppResource
lappHelp s = help
  where help = padTop (Pad 1) $ borderWithLabel (str "Help") body
        body = case (appMode s) of
          AppModeList -> str $ "-   n: New qso       Del: Delete qso\n" <>
                               "- Tab: Edit            s: Sort Log\n" <>
                               "-   w: Write to file   q: Quit"
          AppModeQso -> str $ "- Esc: End edit and accept results\n" <>
                              "- Tab: Cycle entries\n" <>
                              "- Ctrl-l: Lookup name for callsign"
          AppModeSort _ -> str $ "- t: Sort by time\n" <>
                                 "- c: Sort by callsign\n" <>
                                 "- l: Sort by location"
          _ -> str ""



-- | Modes for the UI.
data AppMode = AppModeList     -- ^ Editing the log list.
             | AppModeQso      -- ^ Editing the current QSO entry.
             | AppModeQuestion
               { appModeQuestionPrev :: AppMode,
                 appModeQuestionDialog :: Dialog Bool,
                 appModeQuestionYes :: (AppState -> EventM AppResource AppState),
                 appModeQuestionNo  :: (AppState -> EventM AppResource AppState) } -- ^ Asking a yes/no question from the user.
             | AppModeSort
               { appModeSortPrev :: AppMode }


-- | Event handler for the hamlog application.
lhandleEvent :: AppState -> BrickEvent AppResource HamlogEvent -> EventM AppResource (Next AppState)
lhandleEvent s ev = do

  case (appMode s) of
    AppModeList                     -> lhandleEvent_list s ev
    AppModeQuestion prev dlg yes no -> lhandleEvent_question s ev
    AppModeQso                      -> lhandleEvent_qso s ev
    AppModeSort prev                -> lhandleEvent_sortQsos s ev


lappCursor :: AppState -> [T.CursorLocation AppResource] -> Maybe (T.CursorLocation AppResource)
lappCursor = F.focusRingCursor focusRing


lappStartEvent s = (snd <$> hamLog s readLog) >>= lupdateQsoList 0 >>= lupdateQsoForm
    -- let l = emptyLogState { _stateLog = emptyLog { _logQsos = S.fromList $ take 100000 $ repeat emptyQso } }
    -- let s' = s { logState = l }
    -- lupdateQsoList 0 s' >>= lupdateQsoForm


-- | 'App' to use for constructing the 'Brick' UI.
app = App { appDraw = lappDraw
          , appHandleEvent = lhandleEvent
          , appStartEvent = lappStartEvent
          , appAttrMap = const $ attrMap defAttr
                         [
                           (E.editAttr, V.white `on` V.black)
                         , (E.editFocusedAttr, V.black `on` V.yellow)
                         , (invalidFormInputAttr, V.white `on` V.red)
                         , (focusedFormInputAttr, V.black `on` V.yellow)
                         , (attrName "qsoList" <> attrName "selected", V.black `on` V.yellow)
                         , (attrName "qsoList" <> attrName "normal", defAttr)
                         , (attrName "dupeWarn", V.white `on` V.red)
                         , (buttonAttr,  V.black `on` V.white)
                         , (buttonSelectedAttr, V.black `on` V.green)
                         ]
          , appChooseCursor = lappCursor }

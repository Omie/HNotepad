-- File: HNotepad.hs
-- Simple Notepad using Gtk2hs
-- License - do-whatever-you-want-with-source
--
-- Requires HNotepad.glade in same directory as executable
-- Author - Omie
-- URL - http://intelomkar.wordpress.com/2011/06/07/hnotepad/
-- Twitter - @omkarnath

--import System.Environment
--import Control.Applicative
import System.IO
import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade
--import Maybe

main :: IO ()
main =
    do
        initGUI --required
        gui <- loadGlade "HNotepad.glade" --load gui from glade file
        connectGui gui -- add handlers kind of stuff
        mainGUI

loadGlade :: FilePath -> IO GUI
loadGlade gladepath =
    do
        -- Load XML from glade path.
        -- Note: crashes with a runtime error on console if fails!
        Just xml <- xmlNew gladepath

        -- Load main window
        mw <- xmlGetWidget xml castToWindow "MainWindow"
        windowSetTitle mw "Untitled - HNotepad"
        windowResize mw 400 400
        
        
        --Load MenuItems
        --fileMenu <- xmlGetWidget xml castToMenu "fileMenu"
        [fileNew,fileEdit,fileSave,fileSaveAs,fileQuit] <-
            mapM (xmlGetWidget xml castToImageMenuItem)
            ["FileNew","FileOpen","FileSave","FileSaveAs","FileQuit"]
        
        [editCut,editCopy,editPaste] <-
            mapM (xmlGetWidget xml castToImageMenuItem)
            ["EditCut","EditCopy","EditPaste"]
            
        viewWrap <- xmlGetWidget xml castToCheckMenuItem "ViewWrap"
        viewFont <- xmlGetWidget xml castToImageMenuItem "ViewFont"

        helpAbout <- xmlGetWidget xml castToImageMenuItem "HelpAbout"
        
        --End Load MenuItems
        
        --Load Main Text View
        mainText <- xmlGetWidget xml castToTextView "mainText"
        
        --OpenFileDialog
        ofd <- xmlGetWidget xml castToFileChooserDialog "openFileDialog"
        ofdBtnOk <- xmlGetWidget xml castToButton "ofdBtnOk"
        ofdBtnCancel <- xmlGetWidget xml castToButton "ofdBtnCancel"
        
        --SaveFileDialog
        sfd <- xmlGetWidget xml castToFileChooserDialog "saveFileDialog"
        sfdBtnSave <- xmlGetWidget xml castToButton "sfdBtnSave"
        sfdBtnCancel <- xmlGetWidget xml castToButton "sfdBtnCancel"
        
        --FontSelectionDialog
        fsd <- xmlGetWidget xml castToFontSelectionDialog "fontSelectionDialog"
        fsdBtnCancel <- xmlGetWidget xml castToButton "fsdBtnCancel"
        fsdBtnApply <- xmlGetWidget xml castToButton "fsdBtnApply"
        fsdBtnOk <- xmlGetWidget xml castToButton "fsdBtnOk"
        
        --About Dialog
        abt <- xmlGetWidget xml castToAboutDialog "aboutDialog"
        
        -- Add Extension Filters to Open and Save fileDialogs
        txtfilt <- fileFilterNew
        fileFilterAddPattern txtfilt "*.txt"
        fileFilterSetName txtfilt "Text Files"   
        fileChooserAddFilter ofd txtfilt
        fileChooserAddFilter sfd txtfilt
        
        
        nofilt <- fileFilterNew
        fileFilterAddPattern nofilt "*.*"
        fileFilterSetName nofilt "All Files"
        fileChooserAddFilter ofd nofilt
        fileChooserAddFilter sfd nofilt
        
        --Pack all these things in 1 GUI object and return it
        --this one GUI object will be used everywhere further to
        --access each gui object
        return $ GUI mw
                    fileNew fileEdit fileSave fileSaveAs fileQuit
                    editCut editCopy editPaste
                    viewWrap viewFont
                    helpAbout
                    mainText
                    ofd ofdBtnOk ofdBtnCancel
                    sfd sfdBtnSave sfdBtnCancel
                    fsd fsdBtnCancel fsdBtnApply fsdBtnOk
                    abt
                    
--Custom data type
data GUI = GUI {
        mainWin :: Window,
        fileNew :: ImageMenuItem,
        fileOpen :: ImageMenuItem,
        fileSave :: ImageMenuItem,
        fileSaveAs :: ImageMenuItem,
        fileQuit :: ImageMenuItem,
        editCut :: ImageMenuItem,
        editCopy :: ImageMenuItem,
        editPaste :: ImageMenuItem,
        viewWrap :: CheckMenuItem,
        viewFont :: ImageMenuItem,
        helpAbout :: ImageMenuItem,
        mainText :: TextView,
        ofd :: FileChooserDialog,
        ofdBtnOk :: Button,
        ofdBtnCancel :: Button,
        sfd :: FileChooserDialog,
        sfdBtnSave :: Button,
        sfdBtnCancel :: Button,
        fsd :: FontSelectionDialog,
        fsdBtnCancel :: Button,
        fsdBtnApply :: Button,
        fsdBtnOk :: Button,
        abt :: AboutDialog
      }
      
--Add Handlers kind of stuff
connectGui :: GUI -> IO (ConnectId Button)
connectGui gui =
    do
        -- When the close button is clicked, terminate GUI loop
        -- by calling GTK mainQuit function
        onDestroy (mainWin gui) mainQuit
        
        -- File menu
        onActivateLeaf (fileNew gui) $ newFile gui
        onActivateLeaf (fileOpen gui) $ windowPresent (ofd gui) --openFile1 gui
        onActivateLeaf (fileSave gui) $ saveFile gui
        onActivateLeaf (fileSaveAs gui) $ saveAs gui
        onActivateLeaf (fileQuit gui) mainQuit
        
        --Open and Save As dialogs
        onClicked (ofdBtnOk gui) $ ofdOkBtnClicked gui
        onClicked (ofdBtnCancel gui) $ widgetHide $ ofd gui
        
        onClicked (sfdBtnSave gui) $ sfdSaveBtnClicked gui
        onClicked (sfdBtnCancel gui) $ widgetHide $ sfd gui
        
        --Edit Menu
        onActivateLeaf (editCut gui) $ cut gui
        onActivateLeaf (editCopy gui) $ copy gui
        onActivateLeaf (editPaste gui) $ paste gui
        
        --View Menu
        afterToggle    (viewWrap gui) $ wrap gui
        onActivateLeaf (viewFont gui) $ windowPresent (fsd gui)
        
        --Help Menu
        onActivateLeaf (helpAbout gui) $ windowPresent (abt gui)
        --Font selection dialog
        onClicked (fsdBtnCancel gui) $ widgetHide $ fsd gui
        onClicked (fsdBtnApply  gui) $ fsdApplyBtnClicked gui
        onClicked (fsdBtnOk     gui) $ fsdOkBtnClicked gui

--reset textView
newFile :: GUI -> IO ()
newFile gui = 
    do
        windowSetTitle (mainWin gui) "Untitled - HNotepad"
        buff <- textViewGetBuffer $ mainText gui
        textBufferSetText buff ""

--openFile1 :: GUI -> IO ()
--openFile1 gui =
    --do
        --fileChooserSetAction (ofd gui) FileChooserActionOpen            
        --windowPresent (ofd gui)

-- Get selected file from OFD
-- Read it
-- Program crashes if selected file is not Text file !
ofdOkBtnClicked :: GUI -> IO ()
ofdOkBtnClicked gui =
    do
        file <- fileChooserGetFilename (ofd gui)
        case file of
            Just fpath -> loadFile (show fpath) gui
            Nothing -> widgetHide (ofd gui)
        where
            loadFile fileName gui = 
                do
                    --putStrLn $ init $ tail fileName --filename is enclosed in  "". dont want that
                    inh <- openFile (init  (tail fileName)) ReadMode
                    inputData <- hGetContents inh 
                    buff <- textViewGetBuffer $ mainText gui
                    textBufferSetText buff inputData
                    windowSetTitle (mainWin gui) (init(tail fileName) ++ " - HNotepad")
                    hClose inh
                    widgetHide (ofd gui)        

-- Not Implemented
saveFile :: GUI -> IO ()
saveFile gui = windowSetTitle (mainWin gui) "Save"

saveAs :: GUI -> IO ()
saveAs gui =
    do
        fileChooserSetAction (sfd gui) FileChooserActionSave            
        windowPresent (sfd gui)

-- Init save contents of textView to textFile
sfdSaveBtnClicked :: GUI -> IO ()
sfdSaveBtnClicked gui =
    do
        file <- fileChooserGetFilename (sfd gui)
        case file of
            Just fpath -> save (init(tail(show fpath))) gui
            Nothing -> widgetHide (ofd gui)                        

-- Save contents of textView to textFile
save :: FilePath -> GUI -> IO ()
save fileName gui =
    do
        outh <- openFile fileName WriteMode
        buff <- textViewGetBuffer $ mainText gui
        si <- textBufferGetStartIter buff
        ei <- textBufferGetEndIter buff
        text <- textBufferGetText buff si ei False        
        hPutStrLn outh text
        hClose outh
        widgetHide (sfd gui)
        windowSetTitle (mainWin gui) (fileName ++ " - HNotepad")

-- cut to clipboard
-- not Global keyboard ! only application wide clipboard
cut :: GUI -> IO ()
cut gui =
    do
        sTag <- atomNew "HNotepad"
        clipboard <- clipboardGet sTag
        buff <- textViewGetBuffer $ mainText gui
        textBufferCutClipboard buff clipboard True

--copy to clipboard
copy :: GUI -> IO ()
copy gui =
    do
        sTag <- atomNew "HNotepad"
        clipboard <- clipboardGet sTag
        buff <- textViewGetBuffer $ mainText gui
        textBufferCopyClipboard buff clipboard

--paste from clipboard
paste :: GUI -> IO ()
paste gui =
    do
        sTag <- atomNew "HNotepad"
        clipboard <- clipboardGet sTag
        buff <- textViewGetBuffer $ mainText gui
        textBufferPasteClipboardAtCursor buff clipboard True

-- set text wrap mode for textView
wrap :: GUI -> IO ()
wrap gui =
    do
        state <- checkMenuItemGetActive $ viewWrap gui
        if state then textViewSetWrapMode (mainText gui) WrapWord else
            textViewSetWrapMode (mainText gui) WrapNone        

-- FontSelectorDialog Ok Button
-- Apply Font
-- Close Window
fsdOkBtnClicked :: GUI -> IO ()
fsdOkBtnClicked gui =
    do
        fsdApplyBtnClicked gui
        widgetHide (fsd gui)

-- Apply Font
-- Do NOT close window
fsdApplyBtnClicked :: GUI -> IO ()
fsdApplyBtnClicked gui =
    do
        fontName <- fontSelectionDialogGetFontName $ fsd gui
        case fontName of
            Just fName -> setTextViewFont gui fName
            Nothing -> widgetHide (fsd gui)

-- set font
setTextViewFont :: GUI -> String -> IO ()
setTextViewFont gui fontName =
    do
        fds <-fontDescriptionFromString fontName
        widgetModifyFont (mainText gui) (Just fds)

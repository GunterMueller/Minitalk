Object subclass: #Editor
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Editor class methodsFor: 'editing'!

editString: aString
    "Edit aString using an external file editor. Answer edited string."
    | junk file string |
    junk <- 'junk.mt'.
    file <- File openWrite: junk.
    file write: aString.
    file close.
    self editFile: junk.
    file <- File openRead: junk.
    string <- file readAll.
    file close.
    File delete: 'junk.*'.
    ^string!

editFile: aFilename
    "Edit a file using an external file editor."
    <12>
    ^self primitiveFailed! !

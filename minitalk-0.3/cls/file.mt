Object subclass: #File
    instanceVariableNames: 'handle'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!File class methodsFor: 'private'!

openFile: fileName withMode: fileMode
    ""
    <14>
    ^ self error: 'cannot open file'! !

!File class methodsFor: 'operations on closed files'!

delete: fileName
    ""
    <13>
    ^self error: 'cannot delete file'!

openRead: fileName
    ""
    ^self new handle: (self openFile: fileName withMode: 'rt')!

openWrite: fileName
    ""
    ^self new handle: (self openFile: fileName withMode: 'wt')!

openAppend: fileName
    ""
    ^self new handle: (self openFile: fileName withMode: 'at')!

!

!File methodsFor: 'operations on open files'!

handle: aHandle
  handle <- aHandle!

readAt: anOffset length: aLength
    ""
    <16>
    ^self error: 'cannot read file'!

size
    <18>
    ^self error: 'cannot determine size of file'!

readAll
    <34>
    ^self error: 'cannot read file'!

write: aString
    <17>
    ^self error: 'cannot write file'!

close
    ""
    <15>
    ^self error: 'cannot close file'! !

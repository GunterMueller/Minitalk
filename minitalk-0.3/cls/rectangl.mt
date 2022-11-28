Object subclass: #Rectangle
    instanceVariableNames: 'origin corner'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Rectangle class methodsFor: 'instance creation'!

origin: originPoint corner: cornerPoint
    "Answer a new Rectangle with top left point originPoint
     and bottom right point cornerPoint."
    ^self new origin: originPoint corner: cornerPoint!

origin: originPoint extent: extentPoint
    "Answer a new Rectangle with top left point originPoint
     and bottom right point cornerPoint."
    ^self new origin: originPoint corner: originPoint + extentPoint! !

!Rectangle methodsFor: 'accessing'!

topLeft
    "Answer the Point at the top left corner of the receiver."
    ^origin!

bottomRight
    "Answer the Point at the bottom right corner of the receiver."
    ^origin!

left
    ^origin x!

top
    ^origin y!

right
    ^corner x!

bottom
    ^corner y! !

!Rectangle methodsFor: 'printing'!

printString
    ^origin printString,
     ' corner: ',
     corner printString! !

!Rectangle methodsFor: 'private'!

origin: originPoint corner: cornerPoint
    origin <- originPoint.
    corner <- cornerPoint! !

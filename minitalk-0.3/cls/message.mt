Object subclass: #Message
    instanceVariableNames: 'selector arguments'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Message class methodsFor: 'instance creation'!

selector: aSymbol
    ""
    ^self selector: aSymbol arguments: (Array new: 0)!

selector: aSymbol argument: anObject
    ""
    ^self selector: aSymbol arguments: (Array with: anObject)!

selector: aSymbol arguments: anArray
    ""
    ^self new selector: aSymbol arguments: anArray! !

!Message methodsFor: 'accessing'!

selector
    "Answer the selector of the receiver."
    ^selector!

arguments
    "Answer the arguments of the receiver."
    ^arguments! !

!Message methodsFor: 'private'!

selector: aSymbol arguments: anArray
    selector <- aSymbol.
    arguments <- anArray! !

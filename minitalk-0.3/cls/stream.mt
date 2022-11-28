Object subclass: #Stream
    instanceVariableNames: 'collection'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Stream class methodsFor: 'instance creation'!

on: aCollection
    ^self new on: aCollection! !

!Stream methodsFor: 'accessing - reading'!

next
    ^self subclassResponsibility!

next: anInteger
    | aStream |
    aStream <- WriteStream on: (collection species new: anInteger).
    anInteger timesRepeat: [aStream nextPut: self next].
    ^aStream contents!

nextMatchFor: anObject
    ^self next = anObject!

contents
    | aStream |
    "aStream <- WriteStream on: ()."
    ^self! !

!Stream methodsFor: 'accessing - writing'!

nextPut: anObject
    ^self subclassResponsibility!

nextPutAll: aCollection
    aCollection do: [:element | self nextPut: element].
    ^aCollection!

next: anInteger put: anObject
    anInteger timesRepeat: [self nextPut: anObject].
    ^anObject! !

!Stream methodsFor: 'testing'!

atEnd
    ^self subclassResponsibility! !

!Stream methodsFor: 'enumerating'!

do: aBlock
    [self atEnd] whileFalse:
        [aBlock value: self next]! !

!Stream methodsFor: 'printing'!

printString
    ^self! !

!Stream methodsFor: 'storing'!

storeOn
    ^self! !

!Stream methodsFor: 'private'!

on: aCollection
    collection <- aCollection! !

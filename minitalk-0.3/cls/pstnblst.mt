Stream subclass: #PositionableStream
    instanceVariableNames: 'currentPosition lastPosition'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!PositionableStream class methodsFor: 'instance creation'!

on: aCollection from: firstIndex to: lastIndex
    ^(super on: aCollection) from: firstIndex to: lastIndex!

on: aCollection
    ^self on: aCollection from: 1 to: aCollection size! !

!PositionableStream methodsFor: 'access'!

next
    | anObject |
    self atEnd ifTrue: [^self error: 'stream has no more elements'].
    anObject <- collection at: currentPosition.
    currentPosition <- currentPosition + 1.
    ^anObject!

nextPut: anObject
    self atEnd ifTrue: [^self error: 'stream has no more elements'].
    collection at: currentPosition put: anObject.
    currentPosition <- currentPosition + 1.
    ^anObject!

atEnd
    ^currentPosition > lastPosition! !

!PositionableStream methodsFor: 'private'!

from: first to: last
    currentPosition <- first.
    lastPosition <- last! !

Object subclass: #Magnitude
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Magnitude methodsFor: 'comparing'!

hash
    ^self subclassResponsibility!

= aMagnitude
    ^self subclassResponsibility!

< aMagnitude
    ^self subclassResponsibility!

<= aMagnitude
    ^(self < aMagnitude) or: [self = aMagnitude]!

> aMagnitude
    ^aMagnitude < self!

>= aMagnitude
    ^(self > aMagnitude) or: [self = aMagnitude]!

between: aMagnitude and: anotherMagnitude
    ^(aMagnitude <= self) and: [self <= anotherMagnitude]!

min: aMagnitude
    ^(self < aMagnitude) ifTrue: [self] ifFalse: [aMagnitude]!

max: aMagnitude
    ^(self > aMagnitude) ifTrue: [self] ifFalse: [aMagnitude]! !

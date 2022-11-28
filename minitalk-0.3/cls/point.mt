Object subclass: #Point
    instanceVariableNames: 'x y'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Point class methodsFor: 'instance creation'!

x: xCoord y: yCoord
    "Answer a new Point with coordinates xCoord and yCoord."
    ^self new x: xCoord y: yCoord! !

!Point methodsFor: 'accessing'!

x
    "Answer the x coordinate of the receiver."
    ^x!

x: aNumber
    "Set the x coordinate of the receiver to be aNumber."
    x <- aNumber!

y
    "Answer the y coordinate of the receiver."
    ^y!

y: aNumber
    "Set the y coordinate of the receiver to be aNumber."
    y <- aNumber! !

!Point methodsFor: 'comparing'!

< aPoint
    "Answer whether the receiver is above and to the left of aPoint."
    ^(x < aPoint x) and: [y < aPoint y]!

<= aPoint
    "Answer whether the receiver is neither below nor to the right of aPoint."
    ^(x <= aPoint x) and: [y <= aPoint y]!

> aPoint
    "Answer whether the receiver is below and to the right of aPoint."
    ^(x > aPoint x) and: [y > aPoint y]!

>= aPoint
    "Answer whether the receiver is neither above nor to the left of aPoint."
    ^(x >= aPoint x) and: [y >= aPoint y]!

between: aPoint and: anotherPoint
    "Answer whether the receiver lies right and below aPoint as well
     as left and above anotherPoint, including the point coordinates."
    ^(aPoint <= self) and: [self <= anotherPoint]!

max: aPoint
    "Answer the lower right corner of the rectangle defined
     by the receiver and aPoint."
    ^(x max: aPoint x) @ (y max: aPoint y)!

min: aPoint
    "Answer the upper left corner of the rectangle defined
     by the receiver and aPoint."
    ^(x min: aPoint x) @ (y min: aPoint y)! !

!Point methodsFor: 'arithmetic'!

+ delta
    ^(delta isKindOf: Number)
        ifTrue: [(x + delta) @ (y + delta)]
        ifFalse: [(x + delta x) @ (y + delta y)]!

- delta
    ^(delta isKindOf: Number)
        ifTrue: [(x - delta) @ (y - delta)]
        ifFalse: [(x - delta x) @ (y - delta y)]!

* scale
    ^(scale isKindOf: Number)
        ifTrue: [(x * scale) @ (y * scale)]
        ifFalse: [(x * scale x) @ (y * scale y)]!

/ scale
    ^(scale isKindOf: Number)
        ifTrue: [(x / scale) @ (y / scale)]
        ifFalse: [(x / scale x) @ (y / scale y)]!

negated
    ^x negated @ y negated!

truncated
    ^x truncated @ y truncated! !

!Point methodsFor: 'printing'!

printString
    ^x printString,
     ' @ ',
     y printString! !

!Point methodsFor: 'converting'!

corner: aPoint
    "Answer a Rectangle whose origin is the receiver
     and whose corner is aPoint."
    ^Rectangle origin: self corner: aPoint!

extent: aPoint
    "Answer a Rectangle whose origin is the receiver
     and whose extent is aPoint."
    ^Rectangle origin: self extent: aPoint! !

!Point methodsFor: 'private'!

x: xCoord y: yCoord
    x <- xCoord.
    y <- yCoord! !

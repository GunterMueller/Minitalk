Number variableBinarySubclass: #Float
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Float methodsFor: 'coercing'!

generality
    ^80!

coerce: aNumber
    ^aNumber asFloat!

asFloat
    ^self!

asFraction
    ^self error: 'cannot coerce a Float to a Fraction'!

asSmallInteger
    ^self error: 'cannot coerce a Float to a SmallInteger'! !

!Float methodsFor: 'truncation'!

floor
    <50>
    ^self primitiveFailed!

ceiling
    <51>
    ^self primitiveFailed! !

!Float methodsFor: 'mathematical functions'!

sin
    <52>
    ^self primitiveFailed!

cos
    <53>
    ^self primitiveFailed!

tan
    <54>
    ^self primitiveFailed!

arcSin
    <55>
    ^self primitiveFailed!

arcCos
    <56>
    ^self primitiveFailed!

arcTan
    <57>
    ^self primitiveFailed!

exp
    <58>
    ^self primitiveFailed!

ln
    <59>
    ^self primitiveFailed!

sqrt
    <60>
    ^self primitiveFailed! !

!Float methodsFor: 'arithmetic'!

+ aNumber
    ""
    (aNumber isMemberOf: Float)
        ifTrue: [^self addFloat: aNumber]
        ifFalse: [^self retry: #+ coercing: aNumber]!

- aNumber
    ""
    (aNumber isMemberOf: Float)
        ifTrue: [^self subFloat: aNumber]
        ifFalse: [^self retry: #- coercing: aNumber]!

* aNumber
    ""
    (aNumber isMemberOf: Float)
        ifTrue: [^self mulFloat: aNumber]
        ifFalse: [^self retry: #* coercing: aNumber]!

/ aNumber
    ""
    (aNumber isMemberOf: Float)
        ifTrue: [^self divFloat: aNumber]
        ifFalse: [^self retry: #/ coercing: aNumber]!

< aNumber
    ""
    (aNumber isMemberOf: Float)
        ifTrue: [^self lessFloat: aNumber]
        ifFalse: [^self retry: #< coercing: aNumber]!

= aNumber
    ""
    (aNumber isMemberOf: Float)
        ifTrue: [^self equalFloat: aNumber]
        ifFalse: [^self retry: #= coercing: aNumber]! !

!Float methodsFor: 'arithmetic'!

addFloat: aFloat
    <42>
    ^self primitiveFailed!

subFloat: aFloat
    <43>
    ^self primitiveFailed!

mulFloat: aFloat
    <44>
    ^self primitiveFailed!

divFloat: aFloat
    <41>
    aFloat = 0.0
        ifTrue: [^self error: 'floating division by zero'].
    ^self primitiveFailed!

lessFloat: aFloat
    <40>
    ^self primitiveFailed!

equalFloat: aFloat
    <39>
    ^self primitiveFailed! !

!Float methodsFor: 'printing'!

printString
    ""
    <45>
    ^self primitiveFailed! !

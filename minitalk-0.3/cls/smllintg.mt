Integer subclass: #SmallInteger
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!SmallInteger methodsFor: 'coercing'!

generality
    ^20!

coerce: aNumber
    ^aNumber asSmallInteger!

asFloat
    <46>
    ^self primitiveFailed!

asFraction
    ^Fraction numerator: self denominator: 1!

asSmallInteger
    ^self! !

!SmallInteger methodsFor: 'arithmetic'!

+ aNumber
    ""
    (aNumber isMemberOf: SmallInteger)
        ifTrue: [^self addSmallInteger: aNumber]
        ifFalse: [^self retry: #+ coercing: aNumber]!

- aNumber
    ""
    (aNumber isMemberOf: SmallInteger)
        ifTrue: [^self subSmallInteger: aNumber]
        ifFalse: [^self retry: #- coercing: aNumber]!

* aNumber
    ""
    (aNumber isMemberOf: SmallInteger)
        ifTrue: [^self mulSmallInteger: aNumber]
        ifFalse: [^self retry: #* coercing: aNumber]!

/ aNumber
    ""
    (aNumber isMemberOf: SmallInteger)
        ifTrue: [^self divSmallInteger: aNumber]
        ifFalse: [^self retry: #/ coercing: aNumber]!

< aNumber
    ""
    (aNumber isMemberOf: SmallInteger)
        ifTrue: [^self lessSmallInteger: aNumber]
        ifFalse: [^self retry: #< coercing: aNumber]!

= aNumber
    ""
    (aNumber isMemberOf: SmallInteger)
        ifTrue: [^self equalSmallInteger: aNumber]
        ifFalse: [^self retry: #= coercing: aNumber]!

hash
    ^self! !

!SmallInteger methodsFor: 'arithmetic'!

addSmallInteger: aSmallInteger
    <27>
    ^self primitiveFailed!

subSmallInteger: aSmallInteger
    <28>
    ^self primitiveFailed!

mulSmallInteger: aSmallInteger
    <29>
    ^self primitiveFailed!

divSmallInteger: aSmallInteger
    <26>
    aSmallInteger = 0
        ifTrue: [^self error: 'integer division by zero'].
    ^Fraction numerator: self denominator: aSmallInteger!

lessSmallInteger: aSmallInteger
    <38>
    ^self primitiveFailed!

equalSmallInteger: aSmallInteger
    <37>
    ^self primitiveFailed! !

!SmallInteger methodsFor: 'bit operations'!

bitAnd: aSmallInteger
    <25>
    ^self primitiveFailed! !

!SmallInteger methodsFor: 'printing'!

printString
    ""
    <10>
    ^self primitiveFailed! !

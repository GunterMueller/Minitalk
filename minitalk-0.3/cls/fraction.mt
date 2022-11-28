Number subclass: #Fraction
    instanceVariableNames: 'numerator denominator'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Fraction class methodsFor: 'instance creation'!

numerator: n denominator: d
    "Answer a new Fraction with numerator n and denominator d.
     Use the standard representation for fractions."
    | numer denom factor |
    (n < 0 xor: d < 0)
        ifTrue: [numer <- n abs negated]
        ifFalse: [numer <- n abs].
    denom <- d abs.
    factor <- n gcd: d.
    ^self new numerator: (numer / factor) denominator: (denom / factor)! !

!Fraction methodsFor: 'coercing'!

generality
    ^60!

coerce: aNumber
    ^aNumber asFraction!

asFloat
    ^numerator asFloat / denominator asFloat!

asFraction
    ^self!

asSmallInteger
    ^self error: 'cannot coerce a Fraction to a SmallInteger'! !

!Fraction methodsFor: 'arithmetic'!

+ aNumber
    ""
    (aNumber isMemberOf: Fraction)
        ifTrue: [^self addFraction: aNumber]
        ifFalse: [^self retry: #+ coercing: aNumber]!

- aNumber
    ""
    (aNumber isMemberOf: Fraction)
        ifTrue: [^self subFraction: aNumber]
        ifFalse: [^self retry: #- coercing: aNumber]!

* aNumber
    ""
    (aNumber isMemberOf: Fraction)
        ifTrue: [^self mulFraction: aNumber]
        ifFalse: [^self retry: #* coercing: aNumber]!

/ aNumber
    ""
    (aNumber isMemberOf: Fraction)
        ifTrue: [^self divFraction: aNumber]
        ifFalse: [^self retry: #/ coercing: aNumber]!

< aNumber
    ""
    (aNumber isMemberOf: Fraction)
        ifTrue: [^self lessFraction: aNumber]
        ifFalse: [^self retry: #< coercing: aNumber]!

= aNumber
    ""
    (aNumber isMemberOf: Fraction)
        ifTrue: [^self equalFraction: aNumber]
        ifFalse: [^self retry: #= coercing: aNumber]! !

!Fraction methodsFor: 'arithmetic'!

addFraction: aFraction
    ^Fraction numerator: (numerator * aFraction denominator) +
                         (denominator * aFraction numerator)
              denominator: denominator * aFraction denominator!

subFraction: aFraction
    ^Fraction numerator: (numerator * aFraction denominator) -
                         (denominator * aFraction numerator)
              denominator: denominator * aFraction denominator!

mulFraction: aFraction
    ^Fraction numerator: numerator * aFraction numerator
              denominator: denominator * aFraction denominator!

divFraction: aFraction
    ^Fraction numerator: numerator * aFraction denominator
              denominator: denominator * aFraction numerator!

lessFraction: aFraction
    ^(numerator * aFraction denominator) <
     (denominator * aFraction numerator)!

equalFraction: aFraction
    ^(numerator * aFraction denominator) =
     (denominator * aFraction numerator)! !

!Fraction methodsFor: 'printing'!

printString
    ""
    ^numerator printString, ' / ', denominator printString! !

!Fraction methodsFor: 'component access'!

numerator
    ^numerator!

denominator
    ^denominator! !

!Fraction methodsFor: 'initialization'!

numerator: n denominator: d
    "Set the numerator to n and the denominator to d."
    numerator <- n.
    denominator <- d! !

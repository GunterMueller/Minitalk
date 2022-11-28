Number subclass: #Integer
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Integer class methodsFor: 'test'!

test
   ^self subclass: #Test
         instanceVariableNames: ''
         classVariableNames: ''
         poolDictionaries: ''
         category: ''
   ! !

!Integer methodsFor: 'arithmetic'!

factorial
    | answer |
    answer <- 1.
    1 to: self do: [:i | answer <- answer * i].
    ^answer!

gcd: anInteger
    | a b t |
    a <- self abs.
    b <- anInteger abs.
    [b = 0] whileFalse:
        [t <- a \\ b.
         a <- b.
         b <- t].
    ^a!

lcm: anInteger
    | gcd |
    gcd <- self gcd: anInteger.
    ^self / gcd * anInteger! !

!Integer methodsFor: 'enumerating'!

timesRepeat: aBlock
    | n |
    n <- 1.
    [n <= self] whileTrue:
        [aBlock value.
         n <- n + 1]! !

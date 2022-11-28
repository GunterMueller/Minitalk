Stream subclass: #Random
    instanceVariableNames: 'seed'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Random class methodsFor: 'instance creation'!

new
    ^self basicNew setSeed! !

!Random methodsFor: 'testing'!

atEnd
    ^false! !

!Random methodsFor: 'accessing'!

next
    | temp |
    [seed <- 13849 + (27181 * seed) bitAnd: 16rFFFF.
     temp <- seed / 65536.0.
     temp = 0] whileTrue.
    ^temp! !

!Random methodsFor: 'private'!

setSeed
    seed <- 16rAFFE! !

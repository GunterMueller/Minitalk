ArrayedCollection variableSubclass: #Array
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Array methodsFor: 'printing'!

printString
    ""
    | elementString |
    elementString <- ''.
    self do: [:element | elementString <-
                         elementString, element printString, ' '].
    ^'(', elementString, ')'! !

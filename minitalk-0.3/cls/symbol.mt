String variableBinarySubclass: #Symbol
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Symbol class methodsFor: 'dummy'!

dummy
    ""
    ^self! !

!Symbol methodsFor: 'private'!

species
    ^String! !

!Symbol methodsFor: 'printing'!

printString
    ""
    ^self "asString"! !

!Symbol methodsFor: 'dummy'!

asString
    ""
    | size string |
    size <- self size.
    string <- String new: size.
    1 to: size do: [:i | string at: i put: (self at: i)].
    ^string! !

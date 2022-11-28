ArrayedCollection variableBinarySubclass: #String
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!String class methodsFor: 'input'!

input
    "Input a string from the keyboard."
    <2>
    ^self primitiveFailed! !

!String methodsFor: 'output'!

at: anInteger
    <35>
    ^self primitiveFailed!

at: anInteger put: aCharacter
    <36>
    ^self primitiveFailed!

size
    ""
    <19>
    ^self primitiveFailed!

, aString
    ""
    <8>
    ^self primitiveFailed!

= aString
    <21>
    ^self primitiveFailed!

hash
    <49>
    ^self primitiveFailed!

printString
    ""
    ^'''', self, ''''!

output
    "Output the receiver to the screen."
    <3>
    ^self primitiveFailed! !

Magnitude subclass: #Character
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Character class methodsFor: 'dummy'!

cr
    ""
    ^$
! !

!Character methodsFor: 'dummy'!

isVowel
    ^(self == $a) |
     (self == $e) |
     (self == $i) |
     (self == $o) |
     (self == $u) |
     (self == $A) |
     (self == $E) |
     (self == $I) |
     (self == $O) |
     (self == $U)!

output
    ""
    <11>
    ^self primitiveFailed! !

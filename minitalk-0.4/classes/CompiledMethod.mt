Object subclass: #CompiledMethod
    instanceVariableNames: 'selector primitive numberargs tempsize stacksize bytecodes literals sourceOffset sourceLength'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!CompiledMethod class methodsFor: 'dummy'!

dummy
    ""
    ^self! !

!CompiledMethod methodsFor: 'access'!

disassembleCode
    <61>
    ^self primitiveFailed!

disassemble
    ('<primitive: ', primitive printString, '>') output.
    Character cr output.
    self disassembleCode!

selector
    ^selector!

primitive
    ^primitive!

numberargs
    ^numberargs!

tempsize
    ^tempsize!

stacksize
    ^stacksize!

bytecodes
    ^bytecodes!

literals
    ^literals!

sourceOffset
    ^sourceOffset!

sourceLength
    ^sourceLength! !

!CompiledMethod methodsFor: 'dummy'!

sourceCode
    ""
    | file sourceCode |
    sourceOffset isNil
        ifTrue: [sourceCode <- 'source code is not available']
        ifFalse: [
            file <- File openRead: 'minitalk.src'.
            sourceCode <- file readAt: sourceOffset length: sourceLength.
            file close.
        ].
    ^sourceCode!

sourceCode: aString
    ""
    | file |
    file <- File openAppend: 'minitalk.src'.
    sourceOffset <- file size.
    file write: aString.
    file close.
    sourceLength <- aString size! !

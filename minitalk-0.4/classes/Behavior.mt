Object subclass: #Behavior
    instanceVariableNames: 'superclass methodDictionary characteristic'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Behavior class methodsFor: 'bits in class characteristic'!

hasPointersBit
    ^16r8000!

isIndexableBit
    ^16r4000!

instSizeMask
    ^16r00FF! !

!Behavior methodsFor: 'accessing class hierarchy'!

compiledMethodAt: selector
    ^methodDictionary at: selector!

sourceCodeAt: selector
    ^(methodDictionary at: selector) sourceCode!

sourceMethodAt: selector
    ^(methodDictionary at: selector) sourceCode!

disassembleAt: selector
    ^(methodDictionary at: selector) disassemble!

includesSelector: selector
    ^methodDictionary includesKey: selector!

canUnderstand: selector
    ""
    | class |
    class <- self.
    [class == nil] whileFalse:
        [(class includesSelector: selector) ifTrue: [^true].
         class <- class superclass].
    ^false!

inheritsFrom: aClass
    ""
    | class |
    class <- superclass.
    [class == nil] whileFalse:
        [class == aClass ifTrue: [^true].
         class <- class superclass].
    ^false!

template
    ""
    ^
'
    "What does it do?"
    | temporaries |
    statements
'!

addSelector: aSymbol withMethod: aMethod
    ""
    methodDictionary at: aSymbol put: aMethod!

removeSelector: aSymbol
    ""
    methodDictionary removeKey: aSymbol ifAbsent: []!

editMethod: aSymbol
    ""
    | method oldSourceCode newSourceCode association |
    method <- methodDictionary at: aSymbol ifAbsent: [].
    method isNil
        ifTrue: [oldSourceCode <- aSymbol printString, self template]
        ifFalse: [oldSourceCode <- method sourceCode].
    newSourceCode <- Editor editString: oldSourceCode.
    newSourceCode ~= oldSourceCode
        ifTrue: [
            association <- Compiler compile: newSourceCode in: self.
            association value sourceCode: newSourceCode.
            self addSelector: association key
                 withMethod: association value
        ].
    !

basicNew
    "Create an instance of the receiver."
    <4>
    ^self primitiveFailed!

new
    "Create an instance of the receiver."
    ^self basicNew!

basicNew: anInteger
    "Create an instance of the receiver."
    <5>
    ^self primitiveFailed!

new: anInteger
    "Create an instance of the receiver."
    ^self basicNew: anInteger!

printString
    ^self name!

superclass
    "Answer the superclass of the receiver."
    ^superclass!

methodDictionary
    "Answer the method dictionary of the receiver."
    ^methodDictionary!

characteristic
    "Answer the class characteristic of the receiver."
    ^characteristic!

isPointers
    ^(characteristic bitAnd: Behavior hasPointersBit) ~= 0!

isBinary
    ^(characteristic bitAnd: Behavior hasPointersBit) = 0!

isVariable
    ^(characteristic bitAnd: Behavior isIndexableBit) ~= 0!

isFixed
    ^(characteristic bitAnd: Behavior isIndexableBit) = 0!

instSize
    ^characteristic bitAnd: Behavior instSizeMask! !

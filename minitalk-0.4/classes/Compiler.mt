Object subclass: #Compiler
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Compiler class methodsFor: 'compiling'!

evaluate: aString
    ""
    | association result |
    association <- self compile: 'doIt ', aString
                        in: UndefinedObject
                        lastValueNeeded: true.
    UndefinedObject addSelector: #doIt
                    withMethod: association value.
    result <- nil doIt.
    UndefinedObject removeSelector: #doIt.
    ^result!

compile: aString in: aClass
    ""
    ^self compile: aString
          in: aClass
          lastValueNeeded: false!

compile: aString in: aClass lastValueNeeded: aBoolean
    ""
    <7>
    ^self primitiveFailed! !

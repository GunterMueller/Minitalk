ClassDescription subclass: #Class
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Class methodsFor: 'accessing instances and variables'!

dummy
    ""
    ^self! !

!Class methodsFor: 'instance creation'!

subclass: classSymbol
  instanceVariableNames: stringOfInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolDictNames
  category: categoryNameString
    ""
    | metaclass |
    metaclass <- Metaclass subclassOf: self class.
    ^metaclass name: classSymbol
               inEnvironment: MiniTalk
               subclassOf: self
               instanceVariableNames: stringOfInstVarNames
               variable: false
               pointers: true
               classVariableNames: stringOfClassVarNames
               poolDictionaries: stringOfPoolDictNames
               category: categoryNameString
               comment: ''!

variableSubclass: classSymbol
  instanceVariableNames: stringOfInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolDictNames
  category: categoryNameString
    ""
    | metaclass |
    metaclass <- Metaclass subclassOf: self class.
    ^metaclass new!

variableBinarySubclass: classSymbol
  instanceVariableNames: stringOfInstVarNames
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolDictNames
  category: categoryNameString
    ""
    | metaclass |
    metaclass <- Metaclass subclassOf: self class.
    ^metaclass new! !

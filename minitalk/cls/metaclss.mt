ClassDescription subclass: #Metaclass
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Metaclass class methodsFor: 'metaclass creation'!

subclassOf: superMeta
    "Answer an instance of Metaclass that is a subclass of superMeta."
    ^self new subclassOf: superMeta! !

!Metaclass methodsFor: 'class creation'!

subclassOf: superMeta
    "Initialize the receiver to be a subclass of superMeta."
    superclass <- superMeta.
    methodDictionary <- Dictionary new.
    characteristic <- superMeta characteristic!

name: newName
  inEnvironment: aSystemDictionary
  subclassOf: superClass
  instanceVariableNames: stringOfInstVarNames
  variable: variableBoolean
  pointers: pointerBoolean
  classVariableNames: stringOfClassVarNames
  poolDictionaries: stringOfPoolNames
  category: categoryName
  comment: commentString
    ""
    | class |
    name <- newName.
    class <- self new.
    ^class! !

!Metaclass methodsFor: 'printing'!

name
    ""
    ^super name, ' class'! !

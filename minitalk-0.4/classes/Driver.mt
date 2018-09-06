Object subclass: #Driver
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Driver class methodsFor: 'startup'!

start
    "Start the MiniTalk system."
    ^self new loop!

stop
    "Stop the MiniTalk system."
    <20>! !

!Driver methodsFor: 'read-eval-print loop'!

loop
    "The read-eval-print loop, a most simple user interface."
    | string result |
    [true] whileTrue:
        [string <- String inputWithPrompt: '==> '.
         result <- Compiler evaluate: string.
         result printString output.
         Character cr output.
        ]! !

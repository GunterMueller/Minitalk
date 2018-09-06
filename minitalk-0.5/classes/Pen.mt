Object subclass: #Pen
    instanceVariableNames: 'location direction drawing'
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Pen class methodsFor: 'spirals'!

spiral
    | aPen |
    Graphics on.
    Graphics clear: (245 * 256 + 245) * 256 + 220.
    aPen <- self new. 
    aPen down.
    (1 to: 100) do:
        [:i | aPen go: i; turn: 0.2 pi].
    String input.
    Graphics off! !

!Pen class methodsFor: 'hilbert curves'!

hilbert: n
    | aPen start h |
    Graphics on.
    Graphics clear: (245 * 256 + 245) * 256 + 220.
    aPen <- self new.
    start <- 320 @ 240.
    h <- 384.
    (1 to: n) do:
        [:i | h <- h / 2.
              start <- (start x + (h / 2)) @ (start y + (h / 2)).
              aPen up; goTo: start; down.
              self hilbertAwithPen: aPen order: i width: h].
    String input.
    Graphics off!

hilbertAwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self hilbertDwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ 0.
         self hilbertAwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ h negated.
         self hilbertAwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ 0.
         self hilbertBwithPen: aPen order: i - 1 width: h.
        ]!

hilbertBwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self hilbertCwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ h.
         self hilbertBwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ 0 negated.
         self hilbertBwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ h negated.
         self hilbertAwithPen: aPen order: i - 1 width: h.
        ]!

hilbertCwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self hilbertBwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ 0.
         self hilbertCwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ h.
         self hilbertCwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ 0.
         self hilbertDwithPen: aPen order: i - 1 width: h.
        ]!

hilbertDwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self hilbertAwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ h negated.
         self hilbertDwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ 0.
         self hilbertDwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ h.
         self hilbertCwithPen: aPen order: i - 1 width: h.
        ]! !

!Pen class methodsFor: 'sierpinski curves'!

sierpinski: n
    | aPen start h |
    Graphics on.
    Graphics clear: (245 * 256 + 245) * 256 + 220.
    aPen <- self new.
    start <- 320 @ 240.
    h <- 96.
    start <- (start x) @ (start y + h).
    (1 to: n) do:
        [:i | h <- h / 2.
              start <- (start x - (h * 2)) @ (start y + h).
              aPen up; goTo: start; down.
              self sierpinskiAwithPen: aPen order: i width: h.
              aPen goDelta: h @ h negated.
              self sierpinskiBwithPen: aPen order: i width: h.
              aPen goDelta: h negated @ h negated.
              self sierpinskiCwithPen: aPen order: i width: h.
              aPen goDelta: h negated @ h.
              self sierpinskiDwithPen: aPen order: i width: h.
              aPen goDelta: h @ h].
    String input.
    Graphics off!

sierpinskiAwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self sierpinskiAwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ h negated.
         self sierpinskiBwithPen: aPen order: i - 1 width: h.
         aPen goDelta: (2 * h) @ 0.
         self sierpinskiDwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ h.
         self sierpinskiAwithPen: aPen order: i - 1 width: h.
        ]!

sierpinskiBwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self sierpinskiBwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ h negated.
         self sierpinskiCwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ (2 * h) negated.
         self sierpinskiAwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ h negated.
         self sierpinskiBwithPen: aPen order: i - 1 width: h.
        ]!

sierpinskiCwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self sierpinskiCwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ h.
         self sierpinskiDwithPen: aPen order: i - 1 width: h.
         aPen goDelta: (2 * h) negated @ 0.
         self sierpinskiBwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ h negated.
         self sierpinskiCwithPen: aPen order: i - 1 width: h.
        ]!

sierpinskiDwithPen: aPen order: i width: h
    i > 0 ifTrue:
        [self sierpinskiDwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h @ h.
         self sierpinskiAwithPen: aPen order: i - 1 width: h.
         aPen goDelta: 0 @ (2 * h).
         self sierpinskiCwithPen: aPen order: i - 1 width: h.
         aPen goDelta: h negated @ h.
         self sierpinskiDwithPen: aPen order: i - 1 width: h.
        ]! !

!Pen class methodsFor: 'direction naming'!

north
    ^0.0 pi!

west
    ^1.5 pi!

south
    ^1.0 pi!

east
    ^0.5 pi! !

!Pen class methodsFor: 'instance creation'!

new
    ""
    ^super new up; goTo: 320 @ 240; direction: Pen north! !

!Pen methodsFor: 'drawing'!

down
    drawing <- true!

up
    drawing <- false!

isDown
    ^drawing!

isUp
    ^drawing not!

location
    ^location!

direction
    ^direction!

direction: aDirection
    direction <- aDirection!

turn: amount
    direction <- direction + amount!

go: amount
    | xDelta yDelta |
    xDelta <- amount * direction sin.
    yDelta <- amount negated * direction cos.
    self goTo: (location x + xDelta) @ (location y + yDelta)!

goDelta: aPoint
    self goTo: (location x + aPoint x) @ (location y + aPoint y)!

goTo: aPoint
    drawing ifTrue:
        [Graphics drawLineFrom: location truncated
                  to: aPoint truncated
                  color: 15].
    location <- aPoint! !

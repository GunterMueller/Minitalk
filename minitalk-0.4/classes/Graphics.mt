Object subclass: #Graphics
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: ''!

!Graphics class methodsFor: 'switching modes'!

on
    <248>
    ^self primitiveFailed!

off
    <249>
    ^self primitiveFailed! !

!Graphics class methodsFor: 'line drawing'!

xStart: x1 yStart: y1 xEnd: x2 yEnd: y2 color: color
    <250>
    ^self primitiveFailed!

drawLineFrom: startPoint to: endPoint color: color
    self xStart: startPoint x
         yStart: startPoint y
         xEnd: endPoint x
         yEnd: endPoint y
         color: color! !

!Graphics class methodsFor: 'rectangle drawing'!

xLeft: x1 yTop: y1 xRight: x2 yBottom: y2 color: color
    <251>
    ^self primitiveFailed!

drawRectangle: aRectangle color: color
    self xLeft: aRectangle left
         yTop: aRectangle top
         xRight: aRectangle right
         yBottom: aRectangle bottom
         color: color! !

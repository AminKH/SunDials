# -*- coding: utf-8 -*-
"""
Created on Fri Feb 11 11:10:35 2022

@author: Amin Khiabani
"""

from pyexpat.errors import codes
import FreeCAD
import Draft
from math import pi, cos, sin, sqrt,atan, tan
from PySide import QtGui, QtCore

Gui.activateWorkbench("DraftWorkbench")
Gui.activeDocument().activeView().viewTop()


def drawLine(Points,dialType,lineColor,lineText):
     
    line = Draft.makeWire(Points, closed=False, face=False, support=None)                
    line.ViewObject.LineColor = lineColor   
    if(DialType == "Equatorial_Polar" or DialType == "Equatorial_Vector") : 
        Text = Draft.make_text(lineText, placement=FreeCAD.Vector(Points[0]))
    else:
        Text = Draft.make_text(lineText, placement=FreeCAD.Vector(Points[-1]))  
    Text.ViewObject.FontSize = 2.5  

try: 
    
    Points = []
    li= []
    
    pl=FreeCAD.Placement()
    pl.Rotation.Q=(0.0, 0.0, 0.0, 1.0)
    pl.Base=FreeCAD.Vector(0.0, 0.0, 0.0)

    fileName = QtGui.QFileDialog.getOpenFileName(filter="*.ASC")[0]
   
    if(fileName != '') :   
        file = open(fileName,'r') 
        li = file.readline().rsplit()      
        Location = li[li.index('Location:')+1]
        DialType = li[li.index('Location:')+2]
        Longitude = li[li.index('Longitude')+1]
        Latitude = li[li.index('Latutude')+1]
        Elevation = li[li.index('Elevation')+1]
        timeZone = li[li.index('TimeZone')+1]
        
        li = file.readline().rsplit()       
        Pressure = li[li.index('milibar:')+1]
        Temperature = li[li.index('C:')+1] 

        li = file.readline().rsplit()  
        year = int(li[1]) 

        li = file.readline().rsplit()  
        if(li[0] == 'Gnom'):
            X0 = li[li.index('X0')+1]
            Y0 = li[li.index('Y0')+1]                   
            Length = li[li.index('Length')+1]
            Hight = float(li[li.index('Hight')+1]) 
            angle = li[li.index('plate:')+1]
            beta = float(li[li.index('Horizon:')+1])   

        if(li[0] == 'Radius'): 
            radius = float(li[1])

        if(li[0] == 'Base'):           
            X0 = float(li[li.index('X0')+1])
            Y0 = float(li[li.index('Y0')+1])                 
            Rod1 = float(li[li.index('Rod1')+1])
            Rod2 = float(li[li.index('Rod2')+1])
            Angle = float(li[li.index('East_west')+1])       
     
        li = file.readline().rsplit()  
        if(li[0] == 'Plate'):
           plate = li   

        for line in file:
            li = line.rsplit()              
            if(len(li)==3 ): 
                if(len(Points) >= 2) : 
                    drawLine(Points,DialType,lineColor,lineText) 
                Points = [] 
                if(li[0] == 'Time' or li[0] == 'Date'):           
                    numDays = int(li[1]) 
                    if(li[0] == 'Time' and numDays == 10):  
                        lineText =  li[2] 
                        lineColor = (0.0,0.0,0.0)               
                    elif(li[0] == 'Time' and numDays == 25):                
                        lineText =  li[2] 
                        lineColor =  (0.0,0.0,1.0)         
                    elif(li[0] == 'Date'):                         
                        lineText =  li[1] +"/" + li[2]  
                        lineColor =  (1.0,0.0,0.0)                 
            elif(len(li) == 2):                     
                x = float(li[0])
                y = float(li[1])                           
                Points.append(FreeCAD.Vector(x,y,0.0)) 
                li = []
            elif(len(li) == 0):     
                if(len(Points) >= 2) : 
                    drawLine(Points,DialType,lineColor,lineText) 
                Points = [] 

            if not line:                                
                break
    
except IOError :
        print('Error in reading file')  
        file.close()

file.close()

#try:    

if(DialType == '_Sun_Path') :    
    for r in range(5,95,5) :       
        circle = Draft.makeCircle(radius=r, placement=pl, face=False, support=None) 
        if(r%10 == 0) :
            circle.ViewObject.LineColor = (0.33,0.33,1.00) 
        else:
            circle.ViewObject.LineColor =  (0.67,0.67,1.00)
        Draft.autogroup(circle)

    for angle in range(0,360,10):       
        rangle = angle*pi/180.0
        x1 = 5.0*cos(rangle)
        y1 = 5.0*sin(rangle)
        x2 = 90.0*cos(rangle)
        y2 = 90.0*sin(rangle)
        x3 = 91.5*cos(rangle)
        y3 = 91.5*sin(rangle)
        d = 90.0 - angle 
        if(d < 0) :
            d = 360 + d       
        
        points = [FreeCAD.Vector(x1, y1, 0.0), FreeCAD.Vector(x2, y2, 0.0)]
        line = Draft.makeWire(points, placement=pl, closed=False, face=False, support=None)
        line.ViewObject.LineColor =  (0.00,0.67,0.50)
        line.ViewObject.DrawStyle = u"Dashdot"
        Draft.autogroup(line) 
        text = Draft.make_text(str(d), placement=FreeCAD.Vector(x3,y3, 0.0))
        text.ViewObject.FontSize = 0.5
        Draft.autogroup(text)   

    for Eangle in range(90,0,-5): 
        Etext = Draft.make_text(str(90-Eangle), placement=FreeCAD.Vector(0.0,Eangle, 0.0))
        Etext.ViewObject.FontSize = 0.5
        Draft.autogroup(Etext) 

    pl.Base = FreeCAD.Vector(-100.0, -100.0, 0.0)
    rec = Draft.makeRectangle(length=200.0, height=200.0, placement=pl, face=False, support=None)
    Draft.autogroup(rec)   

    x3 = -95.0
    y3 = 95.0
    t = Location + ' Sungraph ' + str(year)
    text = Draft.make_text(t, placement=FreeCAD.Vector(x3,y3, 0.0))
    text.ViewObject.FontSize = 2.5
    Draft.autogroup(text)        
    
elif(DialType == '_Armillary'):
    pos = [FreeCAD.Vector(0.0, -radius/2.0, 0.0), FreeCAD.Vector(0.0, radius/2.0, 0.0)]
    line = Draft.makeWire(pos, placement=pl, closed=False, face=False, support=None)
    line.ViewObject.LineColor =  (1.00,1.00,0.00)
    line.ViewObject.DrawStyle = u"Dashdot"    
    Draft.autogroup(line) 
    x3 = -5.0
    y3 = radius/2.0+12.5
    t = Location + ' Armillary ' + str(year) + ' Radius: ' + str(radius)
    text = Draft.make_text(t, placement=FreeCAD.Vector(x3,y3, 0.0))
    text.ViewObject.FontSize = 1.5
    Draft.autogroup(text) 

elif(DialType == "_Equatorial" or DialType == "_Equatorial_Vector"):
    points = [FreeCAD.Vector(0.0, -Hight*10, 0.0), FreeCAD.Vector(0.0, Hight*10, 0.0)]
    line = Draft.makeWire(points, placement=pl, closed=False, face=False, support=None)
    line.ViewObject.LineColor =  (1.00,1.00,0.00)
    line.ViewObject.DrawStyle = u"Dashdot" 
    points = [FreeCAD.Vector(-10*Hight, 0.0, 0.0), FreeCAD.Vector(10*Hight, 0.0, 0.0)]  
    line = Draft.makeWire(points, placement=pl, closed=False, face=False, support=None)
    line.ViewObject.LineColor =  (1.00,1.00,0.00)
    line.ViewObject.DrawStyle = u"Dashdot" 
    Draft.autogroup(line) 
    x3 = -4*Hight
    y3 = 10*Hight
    t = Location + ' Equatorial ' + str(year) + ' Gnomon: ' + str(Hight)
    text = Draft.make_text(t, placement=FreeCAD.Vector(x3,y3, 0.0))
    text.ViewObject.FontSize = 1.5
    Draft.autogroup(text)  
    
elif(DialType == "_Bifilar_Horizontal" or DialType == "_Bifilar_Vertical") or DialType == "_Bifilar_Inclined":
    if(DialType == "_Bifilar_Inclined"):
        Angle = 0.0
    circle = Draft.makeCircle(radius=0.5, placement=pl , face=False, support=None) 
    tanAngle = tan(Angle*pi/180.0)
    X1 = 15.0*Rod1
    Y1 = -X1*tanAngle
    Y2 = 4.0*Rod2
    X2= Y2*tanAngle  
    cen=FreeCAD.Placement()    
    cen.Base=FreeCAD.Vector(X0, Y0, 0.0) 
    circle = Draft.makeCircle(radius=0.5, placement=cen , face=False, support=None) 
    pos = [FreeCAD.Vector(-X1, -Y1, 0.0), FreeCAD.Vector(X1, Y1, 0.0)]
    line = Draft.makeWire(pos, placement=pl, closed=False, face=False, support=None)
    line.ViewObject.LineColor =  (1.00,1.00,0.00)
    line.ViewObject.DrawStyle = u"Dashdot"
    pos = [FreeCAD.Vector(-X2, -Y2, 0.0), FreeCAD.Vector(X2, Y2, 0.0)]
    line = Draft.makeWire(pos, placement=pl, closed=False, face=False, support=None)
    line.ViewObject.LineColor =  (1.00,1.00,0.00)
    line.ViewObject.DrawStyle = u"Dashdot"  
    x3 = -3*X0
    y3 = 2*Y0  
    t = Location + ", "+ DialType + str(year) + "Rod1 Hight:" + str(Rod1) + ", Rod2 Hight:" + str(Rod2)
    text = Draft.make_text(t, placement=FreeCAD.Vector(x3,y3, 0.0))
    text.ViewObject.FontSize = 1.5
    Draft.autogroup(text) 

else:   

    points = [FreeCAD.Vector(0.0, -4*Hight, 0.0), FreeCAD.Vector(0.0, 4*Hight, 0.0)]
    line = Draft.makeWire(points, placement=pl, closed=False, face=False, support=None)
    line.ViewObject.LineColor =  (1.00,1.00,0.00)
    line.ViewObject.DrawStyle = u"Dashdot"    
    Draft.autogroup(line)        
       
    if( X0 != "********" and  Y0 != "********"):       
        x0 = float(X0) 
        y0 = float(Y0) 
        if( y0 != 0.0)  :               
            vec1 =   FreeCAD.Vector(x0, y0, 0.0)                  
            points = [FreeCAD.Vector(0.0, 0.0, 0.0), vec1]
            line = Draft.makeWire(points, placement=pl, closed=False, face=False, support=None)  
            line.ViewObject.LineColor =  (0.0,1.0,1.0)
            Draft.autogroup(line) 
            if(x0 != 0.0) :
                m = (atan(y0/x0))
            else:
                m = pi/2.0 
                           
            vec2 = FreeCAD.Vector(Hight*sin(m),-Hight*cos(m), 0.0)
            points = [FreeCAD.Vector(0.0, 0.0, 0.0), vec2]  
            line = Draft.makeWire(points, placement=pl, closed=False, face=False, support=None) 
            line.ViewObject.LineColor =  (0.0,1.0,1.0)    
            Draft.autogroup(line)  
            points = [vec1,vec2]  
            line = Draft.makeWire(points, placement=pl, closed=False, face=False, support=None)  
            line.ViewObject.LineColor =  (0.0,1.0,1.0)  
            Draft.autogroup(line)               
        elif(y0 == 0.0):            
            vec1 =   FreeCAD.Vector(x0, float(Length), 0.0)   
            points = [FreeCAD.Vector(0.0, 0.0, 0.0), vec1]
            line = Draft.makeWire(points, placement=pl, closed=False, face=False, support=None)  
            line.ViewObject.LineColor =  (0.0,1.0,1.0)
            Draft.autogroup(line) 
            x0 = -Hight
            y0 = -Hight
    else:        
        ln = 2.0*Hight
        x0 = ln
        y0 = ln 
        rbeta = beta*pi/180.0    
        x1 = ln*cos(rbeta)
        y1 = ln*sin(rbeta) 
        pos = [FreeCAD.Vector(x1, y1, 0.0), FreeCAD.Vector(-x1, -y1, 0.0)]   
        line = Draft.makeWire(pos, placement=pl, closed=False, face=False, support=None)  
        line.ViewObject.LineColor =  (0.0,1.0,1.0) 
        line.ViewObject.DrawStyle = u"Dashed"
        Draft.autogroup(line) 

    x3 = x0
    y3 = 2.0*y0
    
    t = 'Gnomon Base Xo: '+ X0 + ' , Yo: ' + Y0
    text = Draft.make_text(t, placement=FreeCAD.Vector(x3,y3-12.5, 0.0))
    text.ViewObject.FontSize = 2.5
    t = 'Gnomon Hight: '+ str(Hight) + ' , Style Length: ' + Length  + ' , Angle With Dial Plate: ' + angle
    text = Draft.make_text(t, placement=FreeCAD.Vector(x3,y3-15.0, 0.0))
    text.ViewObject.FontSize = 2.5
    Draft.autogroup(text)   

    t = 'Plate '+ plate[1] + plate[2] + 'Plate '+ plate[3]+plate[4]
    text = Draft.make_text(t, placement=FreeCAD.Vector(x3,y3-20.0, 0.0)) 
    text.ViewObject.FontSize = 2.5
    Draft.autogroup(text)   

t = Location + DialType + str(year)
text = Draft.make_text(t, placement=FreeCAD.Vector(x3,y3, 0.0))
text.ViewObject.FontSize = 3.5 
t = 'Longitude: '+ Longitude + ' , Latitude: ' + Latitude
text = Draft.make_text(t, placement=FreeCAD.Vector(x3,y3-5.0, 0.0))
text.ViewObject.FontSize = 2.5     
t = 'Elevation: '+ Elevation + ' , TimeZone: ' + timeZone
text = Draft.make_text(t, placement=FreeCAD.Vector(x3,y3-7.5, 0.0))
text.ViewObject.FontSize = 2.5
t = 'Atmospheric pressure: '+ str(Pressure) + ' , Temperature: ' + str(Temperature)
text = Draft.make_text(t, placement=FreeCAD.Vector(x3,y3-10.0, 0.0))
text.ViewObject.FontSize = 2.5  
Draft.autogroup(text)   
FreeCAD.ActiveDocument.recompute()
#except:
print("Script ended")

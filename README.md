# SunDials
Program to create table of coordinate (x,y) of shadow tip of gnomon, on sundial surface.
Two methods was used. 

>> 1- Trigonometric method from chapter 58, Calculation of a Planar Sundial in Jean Meeus book, Astronomical Algorithms. 
>> 2- Vector Algebra method. In this method, I assumed that shadow tip is cast by tip of style or gnomon. Therefore, no mater is the shape style, shadow calculated by perpendicular line of style tip to surface of dial.
>> 3- Bifilar sundials calculations are based on book: Sonnenuhren by Beucher.
>> 4 - Armillary dials by myself

These are type of sundials:
>> 0 - Sun Graph 
>> 1-  Equatorial dial(Polar Gnomon)  
>> 2-  Polar dial (Polar Gnomon)  
>> 3-  Horizontal dial(Polar Gnomon) 
>> 4-  Vertical dial (Polar Gnomon) 
>> 5-  Plane Dial(Polar Gnomon) 
>> 11- Bifilar Horizontal 
>> 12- Bifilar Vertical 
>> 21- Equatorial dial(Vector) 
>> 22- Polar dial (Vector)
>> 23- Horizontal dial (Vector) 
>> 24- Vertical dial (Vector) 
>> 25- Plane General Gnomon dial (Vector) 
>> 31- Armillary dial 

The program is by Fortran. The result is saved in text file with, name of location and specs of dial and extension of .ASC. FreeCAD can use this kind of files.
If You run FreeCADSundials.py in FreeCAD macro, the drawing of sundial will be created.
I will be grateful if I have your comments.
This my blog sundialcalculations.blogspot.com . Please send your comments to akhiabani57@gmail.com

//Gnomon height
h = 10;

// Winter Sun Declinations
winAppDec = [-23.44,-20.15,-11.47,0.0];
//Summer Sun Declination
sumAppDec = [0.0,11.47,20.15,23.44];
//Hour Angles
hourAngle = [-90.0,-75.0,-60.0,-45.0,-30.0,-15.0,0.0,15.0,30.0,45.0,60.0,75.0,90.0];

 for(j = [0:1:len(sumAppDec)-1])
      translate([0.0,0.0,h])   
      rotate([sumAppDec[j],0.0,0.0])  
      translate([0,90,0]) color("red")  
      sphere(r = 1 ,$fn = 12);
    
 for(i = [0:1:len(hourAngle)-1])      
    rotate([0.0,0.0,hourAngle[i]])
      for(j=[0:3:3])   
      translate([0.0,0.0,h])   
      rotate([sumAppDec[j],0.0,0.0])  
      translate([0,90,0]) color("red")  
      sphere(r = 1 ,$fn = 12);
      
 for(i = [0:1:len(hourAngle)-1])       
    rotate([0.0,0.0,hourAngle[i]])      
      translate([0.0,0.0,h])   
      rotate([-90.0+sumAppDec[3],0.0,0.0])  
       color("red")  
      cylinder(90.0,0.25,0.25);
 
 for(i = [0:1:len(hourAngle)-1])       
    rotate([0.0,0.0,hourAngle[i]])      
      translate([0.0,0.0,h])   
      rotate([90.0+sumAppDec[3],0.0,0.0])
      color("black")
        cylinder(h/sin(sumAppDec[3]),0.5,0.5);
    
 for(j = [0:1:len(winAppDec)-1])
      translate([0.0,0.0,-h])   
      rotate([winAppDec[j],0.0,0.0])  
      translate([0,90,0]) color("blue")  
      sphere(r = 1 ,$fn = 12);
    
 for(i = [1:1:len(hourAngle)-2])       
    rotate([0.0,0.0,hourAngle[i]])
      for(j=[0:3:3]) 
      translate([0.0,0.0,-h])   
      rotate([winAppDec[j],0.0,0.0])  
      translate([0,90,0]) color("blue")  
      sphere(r = 1 ,$fn = 12);
      
 for(i = [1:1:len(hourAngle)-2])       
    rotate([0.0,0.0,hourAngle[i]])      
      translate([0.0,0.0,-h])   
      rotate([-90.0+winAppDec[0],0.0,0.0])  
      color("blue")  
      cylinder(90.0,0.25,0.25);
    
 for(i = [1:1:len(hourAngle)-2])       
    rotate([0.0,0.0,hourAngle[i]])      
      translate([0.0,0.0,-h])   
      rotate([90.0+winAppDec[0],0.0,0.0])
      color("black")
        cylinder(h/sin(-winAppDec[0]),0.5,0.5);

cylinder(h,0.5,0.0);
rotate([180.0,0.0,0.0])
    cylinder(h,0.5,0.0); 

color("green")
    cube([50,50,1],true);
 
 



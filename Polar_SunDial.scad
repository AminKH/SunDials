//Gnomon height
h = 10;


sunAppDec = [-23.44,-20.15,-11.47,0.0,11.47,20.15,23.44];

hourAngle = [-90.0,-75.0,-60.0,-45.0,-30.0,-15.0,0.0,15.0,30.0,45.0,60.0,75.0,90.0];


 for(j = [0:1:len(sunAppDec)-1])
      translate([0.0,h,0.0])   
      rotate([sunAppDec[j],0.0,0.0])  
      translate([0,90,0]) color("red")  
      sphere(r = 1 ,$fn = 12);
    
 for(i = [0:1:len(hourAngle)-1])
    translate([0.0,h,0.0])       
    rotate([0.0,0.0,hourAngle[i]])
      for(j=[0:3:6])  
      rotate([sunAppDec[j],0.0,0.0])  
      translate([0,90,0]) color("red")  
      sphere(r = 1 ,$fn = 12);      

 for(i = [0:1:len(hourAngle)-1]) 
     translate([0.0,h,0.0])   
     rotate([0.0,0.0,hourAngle[i]]) 
      for(j=[0:3:6])  
      rotate([-90.0+sunAppDec[j],0.0,0.0])
      color("red")       
     cylinder(90.0,0.25,0.25);
 for(i = [1:1:len(hourAngle)-2])
      translate([0.0,h,0.0])    
      rotate([0.0,0.0,hourAngle[i]]) 
      for(j=[0:3:6])
      rotate([90.0+sunAppDec[j],0.0,0.0])
      color("black")
      cylinder(sqrt((h/cos(abs(hourAngle[i])))^2+(h*tan(abs(sunAppDec[j]))/cos(abs(hourAngle[i])))^2),0.5,0.5);
    

rotate([-90.0,0.0,0.0])
    cylinder(h,0.5,0.0); 

 rotate([90.0,0.0,0.0])   
cube([80,40,0.1],true);


//Gnomon height
h = 20;
//Tehran
Lat = 35.755;
//Longitude: 51.375
CoLat = 90.0-Lat;
//Elevation: 1250.0
//Time Zone: 3.5

// Dial Plane
L = 50.0;
W = 60.0;
D = 0.5;

translate([0.0,0.0,sin(CoLat)*L/2.0])
rotate([-CoLat,0.0,0.0])
cube([W,L,D],true);

// Gnomon
translate([0.0,0.0,sin(CoLat)*L/2.0])
rotate([-CoLat,0,0]) 
//color("green") 
cylinder(h,0.25,0.25,true) ;

// Equatorial Sun Path

AzEqu = [89.687175,90.161199,99.049269, 108.784065,120.429440,135.453558,155.431210,180.0,204.586649,224.599085,239.654919,251.326633,261.08569,269.999155,270.555480];
AltEqu = [-0.256395356,0.301254919,12.0603817,23.84662356,34.9154827,44.5427821,51.50474251,54.14233559,51.53573491,44.59690035,34.98598588,23.93077673,12.1583147,0.401588856,-0.266520726];

//echo(len(AzEqu));
for(i = [0:1:len(AzEqu)-1]) 
translate([0.0,0.0,sin(54.245)*L/2.0])   
rotate([AltEqu[i],0,AzEqu[i]]) translate([0,90,0]) color("orange")  
    sphere(r = 1 ,$fn = 12) ;

//  Summer Solitice

SumSolAz = [59.986171,62.499197,70.624782,78.274888,86.064706,94.964462,107.14694,129.405124,180.000000,230.594915,252.853122,265.035609,273.935372,281.725197,289.375309,297.5009,299.989028];
SumSolAlt = [-0.266569,2.515208,13.506706,25.196277,37.231862,49.384125,61.320230,72.108518,77.685467,72.108530,61.320249,49.384149,37.231894,25.196319,13.506761,2.515278,-0.241280];


//echo(len(SumSolAz));
for(i = [0:1:len(SumSolAz)-1])
translate([0.0,h*sin(CoLat)/2,h*cos(CoLat)/2+cos(Lat)*L/2.0]) 
rotate([SumSolAlt[i],0,SumSolAz[i]]) translate([0,90,0]) color("red")   
    sphere(r = 1 ,$fn = 12);
//Sun rays
for(i = [0:1:len(SumSolAz)-1])
translate([0.0,h*sin(CoLat)/2,h*cos(CoLat)/2+cos(Lat)*L/2.0])    
rotate([-90.0+SumSolAlt[i],0,SumSolAz[i]]) 
    color("red")   
    cylinder(90.0,0.25,0.25);
// Shadow Rays
for(i = [1:1:len(SumSolAz)-2])
translate([0.0,h*sin(CoLat)/2,h*cos(CoLat)/2+cos(Lat)*L/2.0])  
rotate([-90.0+SumSolAlt[i],180,180-SumSolAz[i]])
    color("black")   
    cylinder(1.2*h,0.25,0.25);

// Winter Soltice

WinSolAz = [118.692635,126.64669,137.262757,149.772855,164.23083,180.0,195.76907,210.226788,222.736555,233.352262, 241.296415];
WinSolAlt = [-0.266199,8.147850,17.156121,24.39088,29.155459,30.830517,29.155107,24.39023,17.155253,8.14684,-0.257136];;

// Sun Rays
//echo(len(WinSolAz));
for(i = [0:1:len(WinSolAz)-1])   
translate([0.0,-h*sin(CoLat)/2,-h*cos(CoLat)/2+cos(Lat)*L/2.0])   
rotate([WinSolAlt[i],0,WinSolAz[i]]) translate([0,90,0]) color("yellow")  
    sphere(r = 1 ,$fn = 12) ;

for(i = [0:1:len(WinSolAz)-1])
translate([0.0,-h*sin(CoLat)/2,-h*cos(CoLat)/2+cos(Lat)*L/2.0]) 
rotate([-90.0+WinSolAlt[i],0,WinSolAz[i]]) 
     color("yellow")   
    cylinder(90.0,0.25,0.25);
// Shadow Rays
for(i = [2:1:len(WinSolAz)-3])
translate([0.0,-h*sin(CoLat)/2,-h*cos(CoLat)/2+cos(Lat)*L/2.0]) 
rotate([-90.0+WinSolAlt[i],180,180-WinSolAz[i]])
    color("black")   
    cylinder(1.2*h,0.25,0.25);


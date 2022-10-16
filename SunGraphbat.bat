gfortran -Wall  -O2  -ffree-line-length-none -Wall -c SOFAnutate.f95
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c SOFA.f95
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c MoonSun.f95
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c sunDials.f95
gfortran -Wall  -O2  -ffree-line-length-none -Wall -c Main.f90
gfortran SOFAnutate.o SOFA.o MoonSun.o sunDials.o Main.o -O3 -Wextra -Wall -pedantic -ffree-form -static -o sunDial.exe -s

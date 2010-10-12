#!/bin/sh
cd levmar-2.5 && make liblevmar.a && cd ../sundials-2.4.0 && ./configure --prefix=/usr && make
cd ..
sudo cp levmar-2.5/liblevmar.a /usr/lib
sudo cp levmar-2.5/*.h /usr/include
cd sundials-2.4.0
sudo make install

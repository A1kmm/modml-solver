#!/bin/sh
cd levmar-2.5 && make liblevmar.a && cd ../sundials-2.4.0 && ./configure && make

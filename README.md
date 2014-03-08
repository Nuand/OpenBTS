# bladeRF OpenBTS #

Please ensure you have `sync libbladeRF' built and installed. https://github.com/Nuand/bladeRF/tree/dev-libbladeRF_sync
Otherwise, OpenBTS can be compiled by running

## Quick Start ##
1. sudo apt-get install autoconf libtool libosip2-dev libortp-dev libusb-1.0-0-dev g++ sqlite3 libsqlite3-dev erlang libreadline6-dev libncurses5-dev
2. ( cd a53/trunk  ; sudo make install)
3. cd openbts/trunk/
4. ./autogen.sh
5. ./configure --with-bladeRF
6. make -j10
7. sudo mkdir /etc/OpenBTS
8. sudo sqlite3 -init ./apps/OpenBTS.example.sql /etc/OpenBTS/OpenBTS.db ".quit"
9. cd apps
10. ln -s ../Transceiver52M/transceiver transceiver
11. sudo ./OpenBTS


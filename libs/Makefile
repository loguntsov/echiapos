all: chiapos

chiapos: chiapos/.get chiapos/Makefile
	cd chiapos && \
	    make

chiapos/.get:
	git clone https://github.com/Chia-Network/chiapos.git && \
	cd chiapos && \
	    git checkout b014e05f && \
	    touch .get

chiapos/Makefile:
	cd chiapos && \
	    cmake CMakeLists.txt
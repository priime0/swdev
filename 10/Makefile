xscripts:
	-cd ../Q/ && raco pkg remove Q
	cd ../Q/ && raco pkg install --auto --skip-installed
	cd ../10/ && raco make xserver.rkt
	cd ../10/ && chmod +x xserver
	cd ../10/ && raco make xclients.rkt
	cd ../10/ && chmod +x xclients

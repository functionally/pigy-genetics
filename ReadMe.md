PIGY Genetics Game
==================

A simple genetics game using PIGY tokens on Cardano.


Building
--------

Build with the following command:

	nix-build -A pigy-genetics.components.exes.pigy-genetics -o build


Running
-------

Run with the following command:

	gpg -d pigy-genetics.payment.skey.gpg > pigy-genetics.payment.skey &
	build/bin/pigy-genetics testnet.pigy-genetics

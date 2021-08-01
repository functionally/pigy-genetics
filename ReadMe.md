PIGY Genetics Game
==================

A simple genetics game using PIGY tokens on Cardano.

Instructions for using the service are available at https://pigy.functionally.live/.


Building
--------

Build with the following command:

	nix-build -A pigy-genetics.components.exes.pigy-genetics -o build


Configuring
-----------

See [mainnet.pigy-genetics](mainnet.pigy-genetics) or [testnet.pigy-genetics](testnet.pigy-genetics) for example configuration files.


Running
-------

Run with the following command:

	gpg -d pigy-genetics.payment.skey.gpg > pigy-genetics.payment.skey &
	build/bin/pigy-genetics testnet.pigy-genetics


Documentation
-------------

API documentation: https://functionally.github.io/pigy-genetics/.

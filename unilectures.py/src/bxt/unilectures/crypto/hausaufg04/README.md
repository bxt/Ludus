RSA
===

This is a toy implementation of RSA. 

Brute Force
-----------

It features an extra brute force mode to find a readable text that will cipher into a given text.

To start many processes at once on many machines, put the script on a shared file system and run inside a `screen` session:

    cd Dokumente/Ludus/unilectures.py/src/bxt/unilectures/crypto/hausaufg04/
    nice -n 10 python rsa.py

Then datach from the screen and log out. Results will be collected in the results.txt file.

